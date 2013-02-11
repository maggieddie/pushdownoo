package org.ucombinator.dalvik.vmrelated
import org.ucombinator.utils.StringUtils
import org.ucombinator.dalvik.specialAPIs.ExternalLibCallsHandler
import org.ucombinator.utils.CommonUtils



trait SimpleJVMClassReportParser {

  def isMethod(ln: String): Boolean = {
    (ln.contains("public") || 
    ln.contains("private") || 
    ln.contains("protected") || 
    ln.contains("final") || ln.contains("static") ) 
  }
 
  def canThrow(ln: String) : Boolean = {
    ln.contains("throws")
  }
  
 
  
  private def getRest(throwIndx: Int, wholeElems: List[String]) : List[String] = {
    val l  = wholeElems.length
    val exnDot = wholeElems slice(throwIndx+1, l)
    exnDot.map(_.replace(".", "/"))
  }
  
  private def getMethNameOld(methWithArgs: String) : String = {
    val parenthIndx = methWithArgs.indexOf("(")
    val methdot = methWithArgs.substring(0, parenthIndx-1)
    methdot.replace(".", "/")
  }
  
  private def getMethName(throwInd: Int, wholeElems: List[String], elem:String) : String = {
  
     if(throwInd > 0 && ! elem.contains("(")) 
         getMethName(throwInd-1, wholeElems, wholeElems(throwInd -1))
     else{
       val pathInd = elem.indexOf("(")
       val methDot = elem slice(0, pathInd)
       methDot.replace(".", "/")
     }
         
  }
  
 
  
  
  private def getClsName(methName: String) : String = {
    StringUtils.getClassPathFromMethPath(methName)
  }
  
  // right now we only care about method name and exceptions it throws!!!
  def parseLine(ln : String)  {
      val splitElems = splitLine(ln)
      val len = splitElems.length
     
    if(canThrow(ln)){
       val thIdx = splitElems.indexOf("throws")
       val exceptions = getRest(thIdx, splitElems)
       val methWithArgs = splitElems(thIdx -1)
       val methName = getMethName(thIdx, splitElems, methWithArgs)
       val isSpecialAI = if(CommonUtils.isStringLibs(methName) || CommonUtils.isMetaLibCall(methName)){
         true
       }else false
       val clsName = getClsName(methName)
       APISpecs.addAPIEntry(methName,isSpecialAI, "", List(), clsName, "", List(), exceptions)
      }
  else // normal methods without throwing exceptions
      {
        val methWithArgs = splitElems.takeRight(1).head
      
        val methName = getMethName(len-1, splitElems, methWithArgs)
        val isSpecialAI = if(CommonUtils.isStringLibs(methName) || CommonUtils.isMetaLibCall(methName)){
         true
       }else false
          val clsName = getClsName(methName)
             APISpecs.addAPIEntry(methName, isSpecialAI, "", List(), clsName, "", List(), List())
      }
  }

  def splitLine(ln:String): List[String] = {
    ln.split(" ").toList
  }
  

}