package org.ucombinator.utils
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.domains.CommonAbstractDomains.Value
import org.ucombinator.domains.CommonAbstractDomains.D
import org.ucombinator.domains.CommonAbstractDomains.IntentExtraKeyTypeAndValue
 
import org.ucombinator.domains.CommonAbstractDomains.Store
import org.ucombinator.domains.CommonAbstractDomains.StringLit

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
     
     def decideIntentFields(mp: String, clsPath: String, enclosingMethPath: String, argVal: Set[String]) {
       mp match {
         case "android/content/Intent/getAction" => {
            Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getAction"->Set()))
         }
         // This is the thing the fuzzer needs! 
         case "android/content/Intent/getExtras" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getExtras"->Set())) 
         }
          case "android/content/Intent/getStringExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getStringExtra"->Set())) 
         }
         
         case "android/content/Intent/getBooleanArrayExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getBooleanArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getBooleanExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getBooleanArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getByteArrayExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getByteArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getByteExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getByteExtra"->Set())) 
         }
         
         case "android/content/Intent/getCharArrayExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getCharArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getCharExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getCharExtra"->Set())) 
         }
         
         case "android/content/Intent/getCharSequenceArrayExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getCharSequenceArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getCharSequenceArrayListExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getCharSequenceArrayListExtra"->Set())) 
         }
          case "android/content/Intent/getCharSequenceExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getCharSequenceExtra"->Set())) 
         }
            case "android/content/Intent/getDoubleArrayExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getDoubleArrayExtra"->Set())) 
         }
             case "android/content/Intent/getIntArrayListExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getIntArrayListExtra"->Set())) 
         }
              
         case "android/content/Intent/getDoubleExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getDoubleExtra"->Set())) 
         }
         
          case "android/content/Intent/getIntExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getIntExtra"->Set())) 
         }
          
        
           case "android/content/Intent/getFloatArrayExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getFloatArrayExtra"->Set())) 
         }
         
         case "android/content/Intent/getFloatExtra" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getFloatExtra"->Set())) 
         }
         case "android/content/Intent/getPackage" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getPackage"->Set()))
           
         }
         case "android/content/Intent/getData" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getData"->Set()))
         }
         
         case "android/content/Intent/getType" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getType"->Set()))
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
    	  var extraKeyValueTypeMap = Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap.getOrElse((clsPath, enclosingMethPath), Map("getExtras" -> Set()))
    	  var keyVaues = extraKeyValueTypeMap.getOrElse("getExtras", Set.empty[IntentExtraKeyTypeAndValue]).asInstanceOf[Set[IntentExtraKeyTypeAndValue]]
    	  val extraKeyValueTypeMap2 = Map("getExtras" -> (keyVaues + new IntentExtraKeyTypeAndValue(mp, argVal.toString)))
    	  //=  keyVaues + new IntentExtraKeyValueAndType(mp, argVal.toString)
    	  Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += 
    	    ((clsPath, enclosingMethPath) -> extraKeyValueTypeMap2)
       }
      
         case _ => {
           
         } 
     }
     }
     
    
}