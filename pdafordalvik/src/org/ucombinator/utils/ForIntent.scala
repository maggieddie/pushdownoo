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
  
  def bundleAPIs = Set("android/os/Bundle/getInt",
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
      "android/os/Bundle/getString")
      
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
         // will need to stuff the values for the keys of the getExtras 
         case "android/content/Intent/getExtras" => {
           Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap += ((clsPath, enclosingMethPath) -> Map("getExtras"->Set()))
           
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
         
         //getStringExtra
         case "android/os/Bundle/getInt" | 
      "android/os/Bundle/get"| 
      "android/os/Bundle/getBoolean" | 
      "android/os/Bundle/getBooleanArray" | 
      "android/os/Bundle/getBundle"| 
      "android/os/Bundle/getByte"|
      "android/os/Bundle/getByteArray" |  
      "android/os/Bundle/getChar"| 
      "android/os/Bundle/getCharArray" | 
      "android/os/Bundle/getCharSequence" | 
      "android/os/Bundle/getDouble" |
      "android/os/Bundle/getFloat" | 
      "android/os/Bundle/getLong" |
      "android/os/Bundle/getShort"| 
      "android/os/Bundle/getString" => {
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