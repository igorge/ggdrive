package gie.ggdrive

import gie.utils.prop._
import gie.file_utils.file
import java.io.{FileOutputStream, FileInputStream, InputStream, File}
import gie.utils.ImplicitPipe._
import scala.util.Random
import akka.actor._
import com.typesafe.config.ConfigFactory

import scala.concurrent.ExecutionContext.Implicits.global

object Application {

  implicit object messages extends  Messages( new PropsFromClassLoaderBundle("msgs_en.properties") with WithMemo)
  implicit object config extends  Configuration( new PropsFromClassLoaderBundle("application.properties") with WithMemo )

  val KEY_APP = "app"
  val KEY_VERSION = "version"


  def main(args: Array[String]) {
    println(s"${config(KEY_APP)} ${ config(KEY_VERSION)}")


    val gdrive = new GDrive(config(KEY_APP), ()=>this.getClass.getClassLoader.getResourceAsStream(config('client_secrets)),
      ()=>new FileInputStream( new File(config('auth_file)) ),  ()=>new FileOutputStream( new File(config('auth_file)) ))

    //val dataBase = new DataBase(config)

    val fileStore = new FileStore( config )
    val ggQueue = new GDriveQueue( config , fileStore, gdrive )

    var rr = ggQueue.openFile("haha!")
    var rr2 = ggQueue.openFile("haha!")



    val name = "/tmp/mounts/mnt"+Random.nextInt
    new File(name).mkdirs()
    new FUSEDrive(gdrive, ggQueue).mount(name)

    //Thread.sleep(50000)

    //println(rr.value)
//    actorSystem.shutdown()


  }

}