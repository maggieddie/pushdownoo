package org.ucombinator.utils
import org.ucombinator.dalvik.syntax._
import java.io.File

// the class path. method path, will all in string form.


object StringUtils {
  
  

  def main(args: Array[String]): Unit = {
    //val str = "[array [object java/lang/String]]"
    // Debug.prntDebugInfo("",  trimLRPars(str))
   //  System.out.println(str.drop(1)) // test
    // Debug.prntDebugInfo("test args", getArgsFromformals(SName.from("{v0 v1}")))
     
     val arrayPattern = """\(array \(object [^\s]+\)\)""".r
     val clsmethPattern = """([^\s]+(\/[^s])*)""".r
     //clsmethPattern.findAllIn("a/b/c") foreach {println _}
     
     val clsMethP =  "(array (object java/string))"
     println(getClassPathFromMethPath(clsMethP))
     
     val lst = List(1,2,3,4)
    println(  (lst.foldLeft(List[Int]())((res, e)=>{
       (e * 100 :: res)
     })).reverse)
     
     def even(from: Int, to: Int): List[Int] =
       for (i <- List.range(from, to) if i % 2 == 0) yield i
        println(even(0, 20))
     
     println(getMethNameFromMethPath("org/ucombinator/testfileName"))
      println(getClsPathAndFldNameFromFieldPath("org/ucombinator/test$R.fileName"))
      
      println(getTypeFromObjectWrapper("(object dalvik/annotation/Throws)"))
        println(getTypeFromObjectWrapper("(array int)"))
  }
  
  def trimRPar(str: String) : String = str.dropRight(1)
  def trimLPar(str:String) : String = str.drop(1)
  
   def trimLRPars(str: String) : String = trimLPar(trimRPar(str))
   
   def toLstStrFromLstSExp(sns : List[SName]) : List[String]= sns map (_.toString())
  def strToInt(s: String) : BigInt = BigInt(s)
  
  private def getAllRegStrs(startRegStr: String, endRegStr: String) : List[RegisterExp] = {
    val l1 = startRegStr.length
    val l2 = endRegStr.length
    
    
    val startNumStr = startRegStr.substring(1, l1)
    val endNumStr = endRegStr.substring(1, l2)
  
    val startNum = startNumStr.toInt
    val endNum = endNumStr.toInt
    
    if(startNum == endNum)  List(RegisterExp(SName.from(startNumStr)))
    else {
      val indexRange = startNum to endNum
      val indexList = indexRange.toList
      
      val regStrs = indexList.map("v"+_.toString)
      regStrs.map((regStr) => {RegisterExp(SName.from(regStr))})
    }
    
  }
  
  def containsSensitiveStrFrag (input: String) : Boolean = {
	  input.contains("http://") ||
	  input.contains("http:")
	  input.contains("https://") ||
	  input.contains("https:") ||
	  input.contains(".o") || 
	  input.contains(".dex") ||
	  input.contains(".jar") ||
	  input.contains(".cc")
  }
  
  def getArgsFromformals (sx: SExp, isRange: Boolean) : List[RegisterExp] ={
   
      val newRegs = trimLRPars(sx.toString())
      val regStrs = newRegs.split(" ")
    if(isRange){ // gonna contains .. in rnage
        val splitedRes  =  regStrs.toList
      
        val startRegStr = splitedRes.head
        
        val endRegStr  = splitedRes.takeRight(1).head
       
        getAllRegStrs(startRegStr, endRegStr)
      
    }
    else{
     //  val newRegs = trimLRPars(sx.toString())
   
       val arrRegs = regStrs map ((r: String) => new RegisterExp(SName.from(r)))
       CommonUtils.toList(arrRegs)
    }
  }

  // more to test
  def getClassPathFromMethPath(methPath: String): String = {
    val arrayReg = """\(array \(object [^\s]+\)\)""".r
    if (! arrayReg.findAllIn(methPath).toList.isEmpty) {
      System.out.println("array? " + methPath)
      methPath
    } else {
      val elemsArray = methPath.split("/")
      val elemsLst = CommonUtils.toList(elemsArray)
      val clsElems = elemsLst.dropRight(1)
      val slashaddedLst = clsElems.foldLeft("")((res, elem) => {
        res + elem + "/"
      })
    // System.out.println("the get clas mt: " + slashaddedLst)
      trimRPar(slashaddedLst)
    }
  }
  
  def getTypeFromObjectWrapper(str: String) : String = {
    val objectRP = """\(object [^\s]+\)""".r
    if(! objectRP.findAllIn(str).toList.isEmpty) {
      
      val tmpStrLst = trimLRPars(str).split(" ").toList;
      tmpStrLst.drop(1).head
    }
    else{
       str
    }
  }
  
  /**
   * extract classs type and fieldName  from fieldpath
   */
  def getClsPathAndFldNameFromFieldPath(fldPath: String) : (String, String) = {
    val splitRes = fldPath.split("\\.").toList
     (splitRes.dropRight(1).head, splitRes.takeRight(1).head)
   
  }
  
  def getMethNameFromMethPath(methPath: String): String = {
     val arrayReg = """\(array \(object [^\s]+\)\)""".r
    if (! arrayReg.findAllIn(methPath).toList.isEmpty) {
      System.out.println("array? " + methPath)
      methPath
    } else {
      val elemsArray = methPath.split("/")
      val elemsLst = CommonUtils.toList(elemsArray)
      elemsLst.takeRight(1).head
    }
  }
  
  def constrRegStr(num: BigInt) : String = "v"+num.toString
  
   def truncateIfLong(s: String, l: Int): String = {
    if (s == null || l <= 0) s
    else if (s.length() <= l) s
    else s.take(l) + "..."
  }

  def trimFileName(filename: String) = {

    import File.separator

    def trimInternal(name: String) = {
      if (name == null || !name.contains(".") || name.startsWith(".")) {
        name
      } else {
        name.substring(0, name.indexOf("."))
      }
    }

    val nName = trimInternal(filename)

    if (nName == null || !nName.contains(separator) || nName.endsWith(separator)) {
      nName
    } else {
      nName.substring(nName.lastIndexOf(separator) + 1)
    }
  }
  
  def getDistinctMethodOrFieldPath(clsP: String, methP: String, which: String) : String = {
    which match {
      case "fld" => clsP + "."+ methP
      case "meth" => clsP + "/"+ methP
    }
  }
  
  def buildNewMethodIndex(clsName: String, oldMethIndex: CompactMethodIndex) : CompactMethodIndex = {
    val methName = getMethNameFromMethPath(oldMethIndex.methPath)
    val newMethPath = getDistinctMethodOrFieldPath(clsName, methName, "meth")
    CompactMethodIndex(newMethPath, oldMethIndex.argsTypes)
  }
  
  
  def getStringType : String = "java/lang/String"
  
    def stmtContextInfo(clsPath: String, methodPath: String, lineNumber: Stmt)  = {
    "@@@" + clsPath + "$$" + methodPath+ "::" + lineNumber
  }
 
  	
}