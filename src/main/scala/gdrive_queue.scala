package gie.ggdrive

import gie.utils.prop.Configuration
import java.io.{FileNotFoundException, RandomAccessFile, File}
import gie.file_utils.ImplicitFileOps._
import gie.utils.{loan}
import gie.file_utils.file
import scala.collection.mutable
import scala.concurrent.{Promise, Future, ExecutionContext}
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.util.{Failure, Success, Try}
import java.util.concurrent.atomic.AtomicBoolean


package qrec {

  trait QRec

  case class Rec(resourceId: String, inner: QRec)

  case class Noop() extends QRec
  case class Download() extends QRec

}


class GDriveQueue(config: Configuration, store: FileStore, gdrive: GDrive)(implicit executor: ExecutionContext) extends ThreadLocks with LazyLogging { driveQueue=>

  type ResourceId = String

  private val queueRoot = file( config('queue_root) ) / "queue"

  {
    queueRoot.mkdirs()
  }


  private class ResourceQueue {
    type QueueItem = (qrec.QRec, AtomicBoolean, Promise[Any], AtomicBoolean=>Any)
    var current: QueueItem = _
    val queue = new mutable.Queue[QueueItem]()
  }

  private val m_busyResources = new mutable.HashMap[ResourceId, ResourceQueue]()


  private var m_currentQueueRecordId: Long = 1

  private def impl_nextRecordNo() = {
    val r = m_currentQueueRecordId
    m_currentQueueRecordId += 1
    r
  }

  private def impl_CreateRecordFile() = queueRoot / impl_nextRecordNo().toString create_!()

  private def impl_writeRecord[T <: qrec.QRec](id: ResourceId, record: T): qrec.Rec = {

    val tmpRec = qrec.Rec(id, record)
    val tmpRecBin = checksum_serializer.sumFrom( tmpRec )

    loan.acquire( impl_CreateRecordFile() ){ os =>
      os.write(tmpRecBin)
      os.flush()
      os.getFD.sync()
    }

    tmpRec
  }

  //private def impl_sync_scheduleNext(resourceId: ResourceId):Unit = withWriteLock{ impl_ns_scheduleNext(resourceId) }

  private def impl_ns_scheduleNext(resourceId: ResourceId): Unit = {
    m_busyResources.get(resourceId).fold {
      logger.debug(s"[${resourceId}]: no queue, no scheduling next.")
    } {
      resourceQueue =>
        logger.debug(s"[${resourceId}]: scheduling next.")
        if(resourceQueue.current ne null) {
          logger.debug(s"[${resourceId}]: still running task, delaying.")
        } else if (resourceQueue.queue.isEmpty) {
          logger.debug(s"[${resourceId}]: queue is empty, deleting.")
          val r = m_busyResources.remove(resourceId)
          assume(r.isDefined)
        } else {
          val job = resourceQueue.queue.dequeue()
          logger.debug(s"[${resourceId}]: scheduling (${job}).")
          resourceQueue.current = job

          val fun = job._4

          val fut = Future {
            fun(job._2)
          }

          job._3.completeWith( fut )

          fut.onComplete{ r=>
            logger.debug(s"[${resourceId}]: completed with: ${r}")
            driveQueue.withWriteLock{
              resourceQueue.current = null
              impl_ns_scheduleNext(resourceId)
            }
          }

        }
    }
  }

  private def impl_ns_enqueue[T](resourceId: ResourceId, op: qrec.QRec)(fun: AtomicBoolean=>T): Future[T] = {
    val resourceQueue = m_busyResources.getOrElseUpdate(resourceId, new ResourceQueue)
    val prom = Promise[T]()
    val job = ((op, new AtomicBoolean(false), prom.asInstanceOf[Promise[Any]], fun))
    resourceQueue.queue.enqueue(job)
    prom.future
  }

  private def impl_sync_scheduleOpForResource[T](resourceId: ResourceId, op: qrec.QRec)(fun: AtomicBoolean=>T): Future[T] = withWriteLock {
    assume(resourceId ne null)
    assume(resourceId.length>1)

    val fut = impl_ns_enqueue(resourceId, op)(fun)
    impl_ns_scheduleNext(resourceId)

    fut
  }

  private def impl_checkInterrupted(interrupt: AtomicBoolean){
    if(interrupt.get) throw new InterruptedException()
  }

  private def impl_DownloadFile(resourceId: ResourceId):Future[Unit] =
    impl_sync_scheduleOpForResource(resourceId, qrec.Download()) {
      interrupt=>
        logger.debug(s"[${resourceId}]: downloading file.")
        val file = gdrive.fileFromId(resourceId)

        loan.acquire(gdrive.downloadFile(file), store.createFile(resourceId)){ (is, or) =>
          gie.file_utils.copyEx()(is.read _)(or.write _){ impl_checkInterrupted(interrupt) }
        }
    }

  def openFile(resourceId: ResourceId, downloadIfNoLocal: Boolean = true): Future[RandomAccessFile] = withWriteLock {
    Future.fromTry( Try{ store.openFile(resourceId) } ).recoverWith{
      case e:FileNotFoundException if(downloadIfNoLocal) =>  impl_DownloadFile(resourceId) flatMap { _ => openFile(resourceId, false) }
    }
  }





}