package org.ucombinator.utils
import scala.collection.mutable.Map

object TailRecuriveTest {
  abstract class A {
    def next : A
    def next_=(a : A )
  }
  
  object A {
    private val mapA: Map[String, A] = Map.empty

  def register(label: String, lst: A) {
    A.mapA += (label -> lst)}
  def forB(label: String) = mapA.get(label)
  def updateLabelWith(label: String, newLblSt : A) = mapA(label)= newLblSt

  }
  
  case class B(arg: Int, nxt: A) extends A {
    var next  = nxt
   
    override def toString() = arg.toString +"|"+ next.toString
    
    }
  
    case class C(arg: String, nxt: A) extends A {
    var next  = nxt
     A.register(arg, this)
    override def toString() = arg.toString +"|"+ next.toString
    
    }
  
  object End extends A{
    def next = throw new Exception("no more ") 
    def next_=(a: A) { throw new Exception("can't change next on null A") } 
  }
  
  def parseLA(res: List[A], sx: List[A]) : List[A] = {
    sx match {
      case Nil => res
      case hd :: tl => {
        hd match {
          case C(_,_) => {
            parseLA(hd :: res, tl)
          }
          case _ =>{
            parseLA(res,tl)
          }
        }
      }
    }
  }
  
  
  def main(args: Array[String]): Unit = {
    def genStmts(res: List[A])(n : Int): List[A] ={
      if (n == 0) res
      else if(n%2 == 0)
    	  	genStmts(new B(5, End) :: res)(n-1)
    	  	else 
    	  	  	genStmts(new C("A"+n.toString(), End) :: res)(n-1)
    }
    
    val lst = genStmts(List())(8000000)
    parseLA(List(), lst)
  }

}