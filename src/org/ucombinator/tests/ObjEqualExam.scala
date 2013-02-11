package org.ucombinator.tests

object ObjEqualExam {

  case class TestClassKey(s: String, i:Int)  {
    def str = s
    def in = i
    
    override def equals (other : Any) = other match {
      case that : TestClassKey => (that canEqual this ) && (this.str == that.str) && (this.in == that.in)
      case _ => false
    }
    override def hashCode = 41* (41 + in) + in
    override def canEqual(other : Any) = other.isInstanceOf[TestClassKey]
  }
  
  case class SubTestClassKey(ss: String, ii: Int, b : Boolean) extends TestClassKey(ss,ii){
    def bool = b
    override def equals (other : Any) = other match {
      case that : SubTestClassKey => 
        (that canEqual this ) && (this.str == that.str) && (this.in == that.in) && (this.b == that.b)
      case _ => false
    }
    override def hashCode = 41* (41 + in) * in
    override def canEqual(other : Any) = other.isInstanceOf[SubTestClassKey] 
  }
  def main(args: Array[String]): Unit = {
   
    val map = Map.empty[TestClassKey, Int]
     val ins = new TestClassKey("1", 1)
     val map2 = map ++ List((ins->100), (new TestClassKey("2", 2)->200), (new SubTestClassKey("3", 3, true) -> 300))
     val map22 = Map.empty
     val ins22 = new TestClassKey("1", 1)
     val map222 = map ++ List((ins->100), (new TestClassKey("2", 2)->200), (new SubTestClassKey("3", 3, true) -> 300))
        println(map2 == map222) 
        
   val mapLst = Map.empty[List[Int], Int]
      val lst = List(1,2)
      val mapLst2 = mapLst ++ List((List(1,2) -> 100), (List(3,4) -> 300))
      println(mapLst2.get(List(1,2)))
  }
}

        
   /*  val mapt = Map.empty
     val map3 = mapt ++ List((("1", 1)->100), (("2", 2)->200))
      val mapt2 = Map.empty
     val map32 = mapt ++ List((("1", 1)->100), (("2", 2)->200))
     println(map3 == map32)
     println(map3)
     println(map3.get(("1", 1)))*/