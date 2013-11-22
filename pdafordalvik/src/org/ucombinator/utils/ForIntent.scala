package org.ucombinator.utils
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.domains.CommonAbstractDomains.Value
import org.ucombinator.domains.CommonAbstractDomains.D
import org.ucombinator.domains.CommonAbstractDomains.IntentExtraKeyTypeAndValue
import org.ucombinator.domains.CommonAbstractDomains.Store
import org.ucombinator.domains.CommonAbstractDomains.StringLit
import org.ucombinator.dalvik.syntax.Stmt

object ForIntentFuzzerUtil {
  
  
  
//  def tmpIntentAPIs = Set("android/content/Intent/getExtras", 
//      "android/content/Intent/getAction",
//      "android/content/Intent/getType",
//      "android/content/Intent/getPackage",
//      "android/content/Intent/getData"
//      )
  
/*  def bundleAPIs = Set("android/os/Bundle/getInt",
      "android/os/Bundle/get",
      "android/os/Bundle/getBoolean", 
      "android/os/Bundle/getBooleanArray",
      "android/os/Bundle/getBundle",
      "android/os/Bundle/getByte",
      "android/os/Bundle/getByteArray", 
      "android/os/Bundle/getChar",
      "android/os/Bundle/getCharArray", 
      "android/os/Bundle/getCharSequence",
      "android/os/Bundle/getDouble",
      "android/os/Bundle/getFloat",
      "android/os/Bundle/getLong",
      "android/os/Bundle/getShort",
      "android/os/Bundle/getString"  
      )*/
      
  def isMethGetExtra(mp: String) : Boolean = {
    mp == "android/content/Intent/getExtras" 
  }
  
   def isMethGetAction(mp: String) : Boolean = {
    mp == "android/content/Intent/getAction"
  }
   
    def isMethGetData(mp: String) : Boolean = {
    mp == "android/content/Intent/getData"
  }
     def isMethGetType(mp: String) : Boolean = {
    mp == "android/content/Intent/getType"
  }
     
     def isMethGetPkg(mp: String) : Boolean = {
		 mp =="android/content/Intent/getPackage"
  }
     
     def argTVsToMap(argVals: Set[(String, Set[String])], keyOp: String) :  Map[String, Set[IntentExtraKeyTypeAndValue]] = {
       argVals.foldLeft(Map[String, Set[IntentExtraKeyTypeAndValue]]())((res, tyVals) => {
         val typ = tyVals._1
         val valsStr = tyVals._2.toString
         val oneEntryValue = new IntentExtraKeyTypeAndValue(typ, valsStr)
         res + (keyOp -> Set( oneEntryValue))
       })
     }
     
     def decideIntentFields(mp: String, clsPath: String, enclosingMethPath: String, stmt: Stmt, argVal: Set[(String, Set[String])]) {
       mp match {
         case "android/content/Intent/getAction" => {
           val res = 
             argTVsToMap(argVal, "getAction")
            Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
         }
         // This is the thing the fuzzer needs! 
         case "android/content/Intent/getExtras" => {
           
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getExtras"->Set())) 
         }
          case "android/content/Intent/getStringExtra" => {
            val res = 
             argTVsToMap(argVal, "getStringExtra")
            Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
           //Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getStringExtra"->Set())) 
         }
         
         case "android/content/Intent/getBooleanArrayExtra" => {
           val res = 
             argTVsToMap(argVal, "getBooleanArrayExtra")
            Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
           //Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getBooleanArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getBooleanExtra" => {
           val res = 
             argTVsToMap(argVal, "getBooleanExtra")
            Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
          // Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getBooleanArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getByteArrayExtra" => {
             val res = 
             argTVsToMap(argVal, "getByteArrayExtra")
            Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
           //Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getByteArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getByteExtra" => {
            val res = 
             argTVsToMap(argVal, "getByteExtra")
            Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
           //Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getByteExtra"->Set())) 
         }
         
         case "android/content/Intent/getCharArrayExtra" => {
         val res = 
             argTVsToMap(argVal, "getCharArrayExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
          // Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getCharArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getCharExtra" => {
           val res = 
             argTVsToMap(argVal, "getCharArrayExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getCharExtra"->Set())) 
         }
         
         case "android/content/Intent/getCharSequenceArrayExtra" => {
           val res = 
             argTVsToMap(argVal, "getCharSequenceArrayExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
          // Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getCharSequenceArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getCharSequenceArrayListExtra" => {
           val res = 
             argTVsToMap(argVal, "getCharSequenceArrayListExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
         //  Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getCharSequenceArrayListExtra"->Set())) 
         }
          case "android/content/Intent/getCharSequenceExtra" => {
            val res = 
             argTVsToMap(argVal, "getCharSequenceExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
           //Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getCharSequenceExtra"->Set())) 
         }
            case "android/content/Intent/getDoubleArrayExtra" => {
              val res = 
             argTVsToMap(argVal, "getDoubleArrayExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
          // Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getDoubleArrayExtra"->Set())) 
         }
             case "android/content/Intent/getIntArrayListExtra" => {
               val res = 
             argTVsToMap(argVal, "getIntArrayListExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
           //Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getIntArrayListExtra"->Set())) 
         }
              
         case "android/content/Intent/getDoubleExtra" => {
           val res = 
             argTVsToMap(argVal, "getDoubleExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
          // Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getDoubleExtra"->Set())) 
         }
         
          case "android/content/Intent/getIntExtra" => {
            val res = 
             argTVsToMap(argVal, "getIntExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
          // Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getIntExtra"->Set())) 
         }
          
        
           case "android/content/Intent/getFloatArrayExtra" => {
             val res = 
             argTVsToMap(argVal, "getFloatArrayExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
         //  Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getFloatArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getFloatExtra" => {
           val res = 
             argTVsToMap(argVal, "getFloatExtra")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
         //  Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getFloatExtra"->Set())) 
         }
         case "android/content/Intent/getPackage" => {
           val res = 
             argTVsToMap(argVal, "getPackage")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
         //  Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getPackage"->Set()))
           
         }
         case "android/content/Intent/getData" => {
           val res = 
             argTVsToMap(argVal, "getData")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
          // Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getData"->Set()))
         }
         
         case "android/content/Intent/getType" => {
           val res = 
             argTVsToMap(argVal, "getType")
             Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> res)
        //   Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath, stmt) -> Map("getType"->Set()))
         }
         
         //for getExtras, getBundleExtra usually don't use the several APIs?
         case "android/os/Bundle/getInt" | 
         "android/os/Bundle/getIntArray" | 
          "android/os/Bundle/getIntArrayList" | 
      "android/os/Bundle/get"| 
      "android/os/Bundle/getBoolean" | 
      "android/os/Bundle/getBooleanArray" | 
      "android/os/Bundle/getBundle"| 
      "android/os/Bundle/getByte"|
      "android/os/Bundle/getByteArray" |  
      "android/os/Bundle/getChar"| 
      "android/os/Bundle/getCharArray" | 
      "android/os/Bundle/getCharSequence" | 
      "android/os/Bundle/getCharSequenceArray" | 
       "android/os/Bundle/getCharSequenceArrayList" | 
       // getClassLoader
        "android/os/Bundle/getShort" |
      "android/os/Bundle/getShortArray" |
      "android/os/Bundle/getDouble" |
      "android/os/Bundle/getDoubleArray" |
      "android/os/Bundle/getFloat" | 
       "android/os/Bundle/getFloatArray" | 
      "android/os/Bundle/getLong" |
       "android/os/Bundle/getLongArray" |
      "android/os/Bundle/getShort"| 
      "android/os/Bundle/getString" | 
      "android/os/Bundle/getParcelable" |
      "android/os/Bundle/getParcelableArray" |
       "android/os/Bundle/getParcelableArrayList" |
      "android/os/Bundle/getSerializable" | 
      "android/os/Bundle/getSparseParcelableArray" | 
      "android/os/Bundle/getString" |
      "android/os/Bundle/getStringArray" | 
      "android/os/Bundle/getStringArrayList"
      => {
        println("to fill the getExtra map???")
    	  var extraKeyValueTypeMap = Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap.getOrElse((clsPath, enclosingMethPath, stmt), Map("getExtras" -> Set()))
    	  var keyVaues = extraKeyValueTypeMap.getOrElse("getExtras", Set.empty[IntentExtraKeyTypeAndValue]).asInstanceOf[Set[IntentExtraKeyTypeAndValue]]
    	  val extraKeyValueTypeMap2 = Map("getExtras" -> (keyVaues + new IntentExtraKeyTypeAndValue(mp, argVal.toString)))
    	  //=  keyVaues + new IntentExtraKeyValueAndType(mp, argVal.toString)
    	  Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += 
    	    ((clsPath, enclosingMethPath, stmt) -> extraKeyValueTypeMap2)
       }
      
         case _ => {
         } 
     }
     }

    
    
}