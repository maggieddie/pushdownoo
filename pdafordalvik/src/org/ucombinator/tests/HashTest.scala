package org.ucombinator.tests

object HashTest {

    class TestClassKey(s: String, i:Int)  {
    def str = s
    def in = i
    
    override def equals (other : Any) = other match {
      case that : TestClassKey =>// (that canEqual this ) && 
      (this.str == that.str) && (this.in == that.in)
      case _ => false
    }
    override def hashCode = 41* (41 + in) + in
    //override def canEqual(other : Any) = other.isInstanceOf[TestClassKey]
  }
  
   class SubTestClassKey(ss: String, ii: Int, b : Boolean) extends TestClassKey(ss,ii){
    def bool = b
   /* override def equals (other : Any) = other match {
      case that : SubTestClassKey => 
       // (that canEqual this ) &&
     super.equals() && this.b == that.b
      case _ => false
    }*/
    override def hashCode = 41* (41 + in) * in
  //  override def canEqual(other : Any) = other.isInstanceOf[SubTestClassKey] 
  }
  
   abstract class AbstractNumLit
   class NumLit(n: Long) extends AbstractNumLit
   object NumTop extends AbstractNumLit
  def mkNumLit(n: Long): AbstractNumLit = {
    if (n > 2) {
      NumTop
    } else if (n < -2) {
      NumTop
    } else {
      new NumLit(n)
    }
  }
  
  def main(args: Array[String]): Unit = {

    val testKey1 = new TestClassKey("Ineedtofix", 1010)
    val subSetkey = new SubTestClassKey("meto", 2020, true)
    
    val map = Map.empty[TestClassKey, Set[AbstractNumLit]]
    
    val map2 = map ++ List((testKey1 -> Set( new NumLit(7))), (new TestClassKey("2", 2)-> Set( new NumLit(49))), (new SubTestClassKey("3", 3, true) -> Set( new NumLit(49))))
    
    println (map2.hashCode())
      val map1 =     Map.empty[TestClassKey, Set[AbstractNumLit]]
    
    val map3 = map1 ++ List(( new TestClassKey("Ineedtofix", 1010) -> Set( new NumLit(7))), (new TestClassKey("2", 2)-> Set( new NumLit(49))), (new SubTestClassKey("3", 3, true) -> Set( new NumLit(49))))
    println (map3.hashCode())
    
    
  }
  
   
  
 

}