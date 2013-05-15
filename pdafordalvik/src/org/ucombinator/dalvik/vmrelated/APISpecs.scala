package org.ucombinator.dalvik.vmrelated
import scala.collection.immutable.{ Set => ImmSet, Map => ImmMap}
import scala.collection.mutable.Map
import scala.tools.nsc.io.File

object APISpecs extends SimpleJVMClassReportParser{

  /**
   * Should be better read in!!!!
   * Oh, yeah...wait.
   */
 def main(args: Array[String]): Unit = {
   readInReport
   apiSpecTable.foreach(println)
 }
  def EntryPointClassNames: List[String] = {
    List("android/app/Activity ",
      "android/app/Service",
      "android/content/BroadcastReceiver")

  }

  def EntryPointMethodNames: List[String] = {
    val activities = List(
      "onActivityResult",
      "onApplyThemeResource",
      "onBackPressed",
      "onConfigurationChanged",
      "onContextItemSelected",
      "onCreate",
      "onCreateContextMenu",
      "onCreateDialog",
      "onCreateOptionsMenu",
      "onCreatePanelMenu",
      "onCreateView",
      "onDestroy",
      "onKeyDown",
      "onKeyLongPress",
      "onKeyMultiple",
      "onKeyUp",
      "onLowMemory",
      "onMenuItemSelected",
      "onMenuOpened",
      "onNewIntent",
      "onOptionsItemSelected",
      "onPanelClosed",
      "onPause",
      "onPostCreate",
      "onPostResume",
      "onPrepareDialog",
      "onPrepareOptionsMenu",
      "onPreparePanel",
      "onRestart",
      "onRestoreInstanceState",
      "onResume",
      "onSaveInstanceState",
      "onSearchRequested",
      "onStart",
      "onStop",
      "onTitleChanged",
      "onTouchEvent",
      "onWindowFocusChanged")
    activities
  }

  case class APIDesc(
    apiName: String,
    special: Boolean,
    retType: String,
    argTypes: List[String],
    clsName: String,
    pkgName: String,
    permissions: List[String],
    exceptionsThrown: List[String])
    
    /**
     * This will be built up when parsing in the API file
     */
  val apiSpecTable: Map[String, APIDesc] = Map.empty
  
  def readInReport{
    val lines = File("JVMClassReport/cr.txt").lines()
  
    lines.foreach(parseLine)
    setTempApi
  }
  

  def addAPIEntry( 
      apiName: String,
      special: Boolean,
    retType: String,
    argTypes: List[String],
    clsName: String,
    pkgName: String,
    permissions: List[String],
    exceptionsThrown: List[String]) {
    
    val apiDesc = APIDesc(apiName,special, retType, argTypes, clsName, pkgName, permissions, exceptionsThrown)
    apiSpecTable += (apiName -> apiDesc)
  }
  
  // temp use
  def setTempApi {
    addAPIEntry("java/io/FileInputStream/<init>", false, "", List("(object java/lang/String)"), "java/io/FileInputStream", "not known",
        List(), List("java/io/FileNotFoundException"))
  }
  
  def getAPIExns(apiName: String) : List[String] = {
    val apiDescO = apiSpecTable.get(apiName)
    apiDescO match{
      case Some(ad) =>{
        ad.exceptionsThrown
      }
      case  None => { List()}
    }
  }
  
  def isInAPISpecsNameAndArgTypes(callName: String, argTypes: List[String]) : Boolean = {
     val apiDescO = apiSpecTable.get(callName)
    apiDescO match{
      case Some(ad) =>{
        ad.argTypes == argTypes
      }
      case  None => { false}
    }  
  }
  
  def isInAPISpecsbyName(callName: String) : Boolean = {
     val apiDescO = apiSpecTable.get(callName)
    apiDescO match{
      case Some(ad) =>{
        true
      }
      case  None => { false}
    }  
  }
  
  
  

}