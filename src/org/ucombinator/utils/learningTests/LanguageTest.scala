package org.ucombinator.utils.learningTests

object LanguageTest {

  abstract class A {
    def next : A
    def next_=(a : A )
  }
  
  case class B(arg: Int, nxt: A) extends A {
    var next  = nxt
    override def toString() = arg.toString //+"|"+ next.toString
    
    }
  object End extends A{
    def next = throw new Exception("no more ") 
    def next_=(a: A) { throw new Exception("can't change next on null A") } 
  }
  
  def main(args: Array[String]): Unit = {
    def genStmts(res: List[A])(n : Int): List[A] ={
      if (n == 0) res
      else
    	  	genStmts(new B(5, End) :: res)(n-1)
    }
    //val a = new B(1, End)
    //val a2 = new B(2, End)
    val a3 = new B(3, End)
    val a4 = new B(5, End)
    val a5 = new B(10, End)
    
   // System.out.println(a.next)
    //a.next = a2
  //  System.out.println(a.next)
    
    //val lst = List(a, a2)
    
     
     
    val res = linkingList(List())(List(a3, a4, a5))
     System.out.println(res)
     System.out.println("a3.next"+  a3.next)
    val e2 = res.next
    System.out.println(e2)
    //val e3 = e2.next
    // System.out.println(e3)
    
    val res2 = linkedListWrapper(List())(List(a3))
    val res3 = linkedListWrapper(List())(List())
    
    
  }
  
  def linkedListWrapper(res: List[A])(as: List[A]) : Option[A] = {
    if (as.length == 0) None
    else 
    if(as.length == 1)  Some(as.head)
    else Some(linkingList(List())(as))
  }
  
  def linkingList(res: List[A])(as: List[A]) : A = {
    as match {
      case Nil => res.head
      case hd :: tl => { 
        tl match {
          case Nil => {
            res.head
          }
          case hdi :: tli => {
             hd.next = hdi
            linkingList(res ::: List(hd))(tl)
          }
        }
      }
    }
    
  }
}