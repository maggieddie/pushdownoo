/*package org.ucombinator.utils.learningTests

object FrameWorkTest {

   abstract class St(arg1: Int, arg2: String) {
    def argI = arg1
    def argII  = arg2
    
    def argI_= (a: Int)
    def argII_= (s: String)
  }
   
   case class StC1 (arg1: Int, arg2: String, arg3: Boolean) extends St(arg1, arg2){
     def argIII = arg3
   }
   
    case class StC2 (arg1: Int, arg2: String, arg4: Boolean) extends St(arg1, arg2){
      def argIV = arg4
    }
    
    trait TransFormer {
      type state = St
      
      // transform if the first arg is 10
      def trans10 (s: state) : Set[state] ={
        s match {
          case StC1(10, "Hi", false) => Set.empty[state] + new StC1(10, "Hi ", true)
          case (StC2(10, "knoc", true)) => Set.empty[state] +  new StC2(10, "knoc", false)
        }
      }
    }
     not good 
    class C1Transformer extends TransFormer {
      type state = StC1
      
      override def trans10(s: state) : Set[state] = {
        val ss = super.trans10(s) ////shit. it is hard to abstract the high level func!since you can't find the partially 
        //changed staet and continue to modifies it!!!
       // ss.argII = "hi back"
          
      }
    }
}*/