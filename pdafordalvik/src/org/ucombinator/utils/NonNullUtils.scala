package org.ucombinator.utils
import org.ucombinator.dalvik.syntax.DalvikClassDef
import org.ucombinator.dalvik.syntax.FieldAssignStmt
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.dalvik.syntax.Stmt

object NonNullUtils {

   private def inUnNeededClasses(str: String): Boolean = {
    val unNeededClasses = Set[String]("java/lang/String",
      "java/lang/reflect/Field", "java/lang/reflect/Method", 
      "java/io/ObjectOutputStream","java/io/FilePermission","java/io/ObjectStreamClass",
      "java/io/StreamTokenizer", "java/io/PipedReader", "java/io/ObjectInputStream$InputValidationDesc",
      "java/util/GregorianCalendar","java/util/Calendar", "java/util/Currency",
      "java/util/UUID","java/util/Timer", "java/util/Timer", "java/util/SimpleTimeZone",
      "java/util/TimeZone.sxddx", "java/util/Timer$TimerImpl", "java/util/Timer$TimerImpl$TimerHeap","java/lang/ref/ReferenceQueue",
      "java/util/TimerTask","java/util/scanner",  
      "java/util/Timer$FinalizerHelper",
      "java/util/Locale", "java/util/Locale$1")
    unNeededClasses.contains(str)
  }
   
   def shouldNotAnalyze(clsDef : DalvikClassDef) : Boolean = {
     val clsName = clsDef.clsPath
     val clsAttrs = clsDef.attrs
     
     val cond1 = inUnNeededClasses(clsName) 
     val cond2 = clsAttrs.contains("abstract") 
     val cond3 = clsDef.getDirectObjFields.isEmpty
     println(cond1 + "|" + cond2 + "|" + cond3)
     // not our concern
     cond1 || 
     // no instantiation of abstract class
     cond2  || 
     // no objects fields, no bother to analyze
    cond3
   }
   
   def trackNullRefDuringConstrs(stmt: Stmt) {
      var cnt = Thread.currentThread().asInstanceOf[AnalysisHelperThread].nullRefMap.getOrElse(stmt, 0) 
        Thread.currentThread().asInstanceOf[AnalysisHelperThread].nullRefMap = 
          Thread.currentThread().asInstanceOf[AnalysisHelperThread].nullRefMap + (stmt -> (cnt +1))
   }
}