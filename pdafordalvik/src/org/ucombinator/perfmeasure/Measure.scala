package org.ucombinator.perfmeasure

object Measure {
  def measureTime[A] (prefix : Object) (code: => A) : A = {
    val start = System.currentTimeMillis() ;
    val ret = code
    val end = System.currentTimeMillis() ;
    println(prefix + "" + (end-start) + "ms" )
    ret
  }

  def repeat (n : Int) (code: => Unit) {
    var i = n
    while (i > 0) {
      code
      i -= 1
    }
  }
}