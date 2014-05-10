package gie.utils.swing

import javax.swing.SwingUtilities
import scala.concurrent.{ExecutionContext, Promise, Future}
import scala.util.Try

package object ui_thread {

  def assumeUIThread() = assume( SwingUtilities.isEventDispatchThread() )
  def assumeNonUIThread() = assume( !SwingUtilities.isEventDispatchThread() )
  def inUI[T](fun: =>T):Future[T] =
    if(SwingUtilities.isEventDispatchThread)
      Promise[T]().complete( Try(fun) ).future
    else {
      val promise = Promise[T]()
      SwingUtilities.invokeLater(new Runnable(){ def run{promise complete Try(fun) }})
      promise.future
    }

  def invokeLaterInUI[T](fun: =>T):Future[T] = {
    val promise = Promise[T]()
    SwingUtilities.invokeLater(new Runnable(){ def run{promise complete Try(fun) }})
    promise.future
  }

  def outUI[T](fun: =>T)(implicit executor: ExecutionContext): Future[T] =
    if(SwingUtilities.isEventDispatchThread)
      Future(fun)
    else
      Promise[T]().complete( Try(fun) ).future

  implicit class FatalOnFail[T](val fut: Future[T])(implicit executor: ExecutionContext){
    def fatalOnFail:Future[T] = fut.recover {
      case e @ _ => e.printStackTrace
        sys.exit(1)
    }

  }


}