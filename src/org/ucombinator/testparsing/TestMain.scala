package org.ucombinator.testparsing

import org.ucombinator.dalvik.syntax._
import java.io.File
import org.ucombinator.dalvik.parsing.S2DParser
import org.ucombinator.utils.Debug
import org.ucombinator.dalvik.syntax.CompactMethodIndex
import org.ucombinator.utils.CommonUtils
import org.ucombinator.playhelpers.AnalysisHelperThread

object TestMain {

  def main(args: Array[String]): Unit = {
    
 //  val testFileName = "./sexpr/BuildConfig.sxddx";
   //val sexp = SExp.parseAllIn(testFileName)
      //System.out.println(sexp)
    
    testParsing("./tests/sexps_antlr")
   // test_exception()
    //val testFP = "./tests/sexps_ucm/UltraCoolMapActivity.sxddx"
  // test_pp()
      //val testF = new java.io.File(testFP)
      //simplefunc(testF)
      val onstart = DalvikClassDef.lookupMethod("antlr/preprocessor/Tool","antlr/preprocessor/Tool/main",List("(array (object java/lang/String))"), 0)
      println(onstart)
  }
  
 ///Volumes/MI/wk_ef/pdafordalvik/tests/sexps_ucm/UltraCoolMapActivity.sxddx
   def simplefunc (path:File) {
      val fp = path.getAbsolutePath();
      Debug.prntDebugInfo("parsing file>>>>>>>", fp)
      val sexp = SExp.parseAllIn(fp)
      
      testASTParser(sexp)
      //println(sexp)
     // System.err.println(sexp)
      System.err.println("done")
    }
  
  def testParsing (dirname:String) {
    val arrFiles = new java.io.File(dirname).listFiles.filter(_.getName.endsWith(".sxddx"))

   
    
    arrFiles.foreach(simplefunc)
  }
  
  
  def testASTParser (input: List[SExp]){
   
    
    val astBeforeAnf = S2DParser(input);
    
    
     
   
    
  }
  
  // dumb private def to find the static method.
  private def getFactStaticStmt(clsd: DalvikClassDef): Option[MethodDef] = {
    val compactMethIndex =  new CompactMethodIndex("com/android/demo/notepad3/NoteEdit/factorial", List("int"))
    clsd.lookupCurClsMethTable(compactMethIndex)
  }
  
 final case class tstLst (car: Int, cdr :List[Int])  {
     		def toList = car :: cdr.toList
  }
 
 private def test_exception() {
   val onstart = DalvikClassDef.lookupMethod("com/invincea/draw/MyDrawService","com/invincea/draw/MyDrawService/onStart",List("(object android/content/Intent)","int"), 1)
   System.err.println("can find the onStart?" + " " + onstart)
          CommonUtils.flattenLinkedStmt(List())(onstart.head.body).foreach(println)
          
          val factbody = onstart.head.body
           val f1 = factbody.next
          System.out.println("f1: " + f1)
           val f2 = f1.next
          System.out.println("f2: " + f2)
           val f3 = f2.next
          System.out.println("f3: " + f3)
           val f4 = f3.next
          System.out.println("f4: " + f4)
          
               val f5 = f4.next
          System.out.println("f5: " + f5)
            
               val f6 = f5.next
          System.out.println("f6: " + f6)
            
               val f7= f6.next
          System.out.println("f7: " + f7)
                         val f8= f7.next
          System.out.println("f8: " + f8)
                         val f9= f8.next
          System.out.println("f9: " + f9)
                 val f10 = f9.next
          System.out.println("f10: " + f10)
            
               val f11= f10.next
          System.out.println("f11: " + f11)
                         val f12= f11.next
          System.out.println("f12: " + f12)
                         val f13= f12.next
          System.out.println("f13: " + f13)
          
                val f14= f13.next
          System.out.println("f14: " + f14)
                         val f15= f14.next
          System.out.println("f15: " + f15)
                         val f16= f15.next
          System.out.println("f16: " + f16)
          
                     val f17= f16.next
          System.out.println("f17: " + f17)
                         val f18= f17.next
          System.out.println("f18: " + f18)
                         val f19= f18.next
          System.out.println("f19: " + f19)
          
                   val f20 = f19.next
          System.out.println("f20: " + f20)
            
               val f21= f20.next
          System.out.println("f21: " + f21)
                         val f22= f21.next
          System.out.println("f22: " + f22)
                         val f23= f22.next
          System.out.println("f23: " + f23)
          
                val f24= f23.next
          System.out.println("f24: " + f24)
                         val f25= f24.next
          System.out.println("f25: " + f25)
                         val f26= f25.next
          System.out.println("f26: " + f26)
          
                     val f27= f26.next
          System.out.println("f27: " + f27)
                         val f28= f27.next
          System.out.println("f28: " + f28)
                         val f29= f28.next
          System.out.println("f29: " + f29)
 
 }
 
 private def test_pp() {
   
   val pp = DalvikClassDef.lookupMethod("org/ucomb/tests/TestExn2","org/ucomb/tests/TestExn2/blowUp",List( ), 0)
   val pp2= DalvikClassDef.lookupMethod("org/ucomb/tests/TestExn2","org/ucomb/tests/TestExn2/normalTryCatchRethrow",List(), 0)
     
    CommonUtils.flattenLinkedStmt(List())(pp.head.body).foreach(println)
   println( pp.head.localHandlers  + " ::: " + pp.head.annotationExns + " ::: " )
   println( pp2.head.localHandlers +  " ::: "+  pp2.head.annotationExns)
   
  /*  val pp3 = DalvikClassDef.lookupMethod("org/ucomb/tests/TestExns","org/ucomb/tests/TestExns/fTryCatchFinally1",List("(object java/lang/String)" ), true)
   val pp4= DalvikClassDef.lookupMethod("org/ucomb/tests/TestExns","org/ucomb/tests/TestExns/fTryReturnCatchNOReturn",List( "(object java/lang/String)"), true)
    val pp5= DalvikClassDef.lookupMethod("org/ucomb/tests/TestExns","org/ucomb/tests/TestExns/fthrs",List( "(object java/lang/String)"), true)
   
    println( pp3.head.localHandlers  + " ::: " + pp3.head.annotationExns + " ::: " )
   println( pp4.head.localHandlers +  " ::: "+  pp4.head.annotationExns)
    println( pp5.head.localHandlers +  " ::: "+  pp5.head.annotationExns)
    */
   //  CommonUtils.flattenLinkedStmt(List())(pp.head.body).foreach(println)
     
     /* val factbody = pp.head.body
           val f1 = factbody.next
          System.out.println("f1: " + f1)
           val f2 = f1.next
          System.out.println("f2: " + f2)
           val f3 = f2.next
          System.out.println("f3: " + f3)
           val f4 = f3.next
          System.out.println("f4: " + f4)
          
               val f5 = f4.next
          System.out.println("f5: " + f5)
            
               val f6 = f5.next
          System.out.println("f6: " + f6)
            
               val f7= f6.next
          System.out.println("f7: " + f7)
                         val f8= f7.next
          System.out.println("f8: " + f8)
                         val f9= f8.next
          System.out.println("f9: " + f9)
                 val f10 = f9.next
          System.out.println("f10: " + f10)
            
               val f11= f10.next
          System.out.println("f11: " + f11)
                         val f12= f11.next
          System.out.println("f12: " + f12)
                         val f13= f12.next
          System.out.println("f13: " + f13)
          
                val f14= f13.next
          System.out.println("f14: " + f14)
                         val f15= f14.next
          System.out.println("f15: " + f15)
                         val f16= f15.next
          System.out.println("f16: " + f16)
          
                     val f17= f16.next
          System.out.println("f17: " + f17)
                         val f18= f17.next
          System.out.println("f18: " + f18)
                         val f19= f18.next
          System.out.println("f19: " + f19)
          
                   val f20 = f19.next
          System.out.println("f20: " + f20)
            
               val f21= f20.next
          System.out.println("f21: " + f21)
                         val f22= f21.next
          System.out.println("f22: " + f22)
                         val f23= f22.next
          System.out.println("f23: " + f23)
          
                val f24= f23.next
          System.out.println("f24: " + f24)
                         val f25= f24.next
          System.out.println("f25: " + f25)
                         val f26= f25.next
          System.out.println("f26: " + f26)
          
                     val f27= f26.next
          System.out.println("f27: " + f27)
                         val f28= f27.next
          System.out.println("f28: " + f28)
                         val f29= f28.next
          System.out.println("f29: " + f29)
          
                    val f30 = f29.next
          System.out.println("f29: " + f30)
            
               val f31= f30.next
          System.out.println("f31: " + f31)
                         val f32= f31.next
          System.out.println("f32: " + f32)
                         val f33= f32.next
          System.out.println("f33: " + f33)
          
                val f34= f33.next
          System.out.println("f34: " + f34)
                         val f35= f34.next
          System.out.println("f35: " + f35)
                         val f36= f35.next
          System.out.println("f36: " + f36)
          
                     val f37= f36.next
          System.out.println("f37: " + f37)
                         val f38= f37.next
          System.out.println("f38: " + f38)
                         val f39= f38.next
          System.out.println("f39: " + f39)*/
 }
 private def test_fact() {
    val fact = DalvikClassDef.lookupMethod("com/android/demo/notepad3/NoteEdit","com/android/demo/notepad3/NoteEdit/factorial",List( "int"), 0)
     System.err.println("can find the factorial?" + " " + fact)
          System.err.println(CommonUtils.flattenLinkedStmt(List())(fact.head.body) )
          
          val factbody = fact.head.body
          

          val f1 = factbody.next
          System.out.println("f1: " + f1)
           val f2 = f1.next
          System.out.println("f2: " + f2)
           val f3 = f2.next
          System.out.println("f3: " + f3)
           val f4 = f3.next
          System.out.println("f4: " + f4)
          
               val f5 = f4.next
          System.out.println("f5: " + f5)
            
               val f6 = f5.next
          System.out.println("f6: " + f6)
            
               val f7= f6.next
          System.out.println("f7: " + f7)
                         val f8= f7.next
          System.out.println("f8: " + f8)
                         val f9= f8.next
          System.out.println("f9: " + f9)
                 val f10 = f9.next
          System.out.println("f10: " + f10)
            
               val f11= f10.next
          System.out.println("f11: " + f11)
                         val f12= f11.next
          System.out.println("f12: " + f12)
                         val f13= f12.next
          System.out.println("f13: " + f13)
          val lbTable =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].stmtMap.values
   
 }
 
  
}