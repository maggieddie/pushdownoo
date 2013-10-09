package org.ucombinator.playhelpers
import java.io.File

object IRExtractHelper {

  var parseThread : ExtractIRHelperThread = null
  
   def parseInApk(filePath: String, doNotNull: Boolean) : (String, String)  = {
      println("filePath" + filePath)
      val lst = filePath.split("/").toList
      val plen = lst.length
      //println(plen)
      
      /*val fileName = lst(plen-1)
        println(fileName)
      val lst2 = fileName.split("\\.").toList
      val plen2 = lst2.length
      val fileFoldnerName = lst2(plen2-2) */
      val fileParts = filePath.split("\\.apk").toList
      val fileFoldnerName = fileParts.head 
      val fileName = fileParts.tail
      
      println("Project Name to Analyzer::::::::::::: "+ fileFoldnerName)
     
      println("-----tail: ", fileName)
      val pathToScript  = lst.dropRight(1).foldLeft("")((res, s) => {res + s + "/"})
      
     val getIRCmdStr = 
       if(doNotNull) {
         "/usr/bin/python ./getIR.py" + " " + "--donull" + " " + pathToScript
       } else{
         "/usr/bin/python ./getIR.py" + " " + "--nonull" +  " " + pathToScript
       }//"/usr/bin/python ./getIR.py" + " " +  pathToScript
     val et: ExtractIRHelperThread = new ExtractIRHelperThread(getIRCmdStr)
      IRExtractHelper.parseThread = et
      et.start() 
      
      var projFolder = fileFoldnerName 
      var  irfolder= fileFoldnerName + File.separator + "dedexout"
      (irfolder, projFolder)  
    }
 
  // test 
    def main(args: Array[String]) :Unit = {
        val curDir = System.getProperty("user.dir") 
        parseInApk("/Users/shuying/Documents/bk/wk_if/pdcfaexp/test/core-classes.apk", true) 
        IRExtractHelper.parseThread.join()
        println("finished") 
    }
}