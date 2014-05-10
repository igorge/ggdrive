package gie

package object cp {

  val NULLCP = -1
  def from(data: String) = new StringToCPIterator(data)

  class StringToCPIterator(data: String) extends Iterator[Int] {
    private var currentPos = 0
    private def feedCP(): Int = {
      assume(currentPos<=data.size)
      if(currentPos==data.size) {
        impl_throwNoMoreItemsException()
      } else {
        val currentCP = Character.codePointAt(data, currentPos)
        assume(currentCP!=NULLCP)
        currentPos += Character.charCount(currentCP)
        currentCP
      }
    }
    def hasNext: Boolean = {
      currentPos!=data.size
    }
    def next(): Int = feedCP()

    private def impl_throwNoMoreItemsException():Nothing = {
      throw new IllegalStateException()
    }
  }



}