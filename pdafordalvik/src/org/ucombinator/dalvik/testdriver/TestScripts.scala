package org.ucombinator.dalvik.testdriver
 import scala.tools.nsc.io._
  //import java.io._
    import scala.actors._
    import scala.actors.Actor._

    
object TestScripts {
   
 
  /**
   * NOT QUITE CONVINIENT, DEPRECIATED
   */
    private val caller = self
    private val WAIT_TIME = 100000
 
    private val reader = actor {
        println("created actor: " + Thread.currentThread)
        var continue = true
        loopWhile(continue){
            reactWithin(WAIT_TIME) {
                case TIMEOUT =>
                    caller ! "react timeout"
                case proc:Process =>
                    println("entering first actor " + Thread.currentThread)
                    val streamReader = new java.io.InputStreamReader(proc.getInputStream)
                    val bufferedReader = new java.io.BufferedReader(streamReader)
                    val stringBuilder = new java.lang.StringBuilder()
                    var line:String = null
                    while({line = bufferedReader.readLine; line != null}){
                        stringBuilder.append(line)
                        stringBuilder.append("\n")  
                    }
                    bufferedReader.close
                    caller ! stringBuilder.toString
            }
        }
    }
 
    def runSysCmd(command:String) : Int = {
        println("gonna runa a command: " + Thread.currentThread)
        val args = command.split(" ")
        val processBuilder = new ProcessBuilder(args: _* )
        processBuilder.redirectErrorStream(true)
        val proc = processBuilder.start()
         
 
        //Send the proc to the actor, to extract the console output.
        reader ! proc
 
        //Receive the console output from the actor.
        receiveWithin(WAIT_TIME) {
                case TIMEOUT => { 
                  println("receiving Timeout") 
                  0}
                case result:String => {
                  println("print result")
                  println(result)
                  1
                }
        }
    }
    // a/b/c.apk
    // will return the apk foleder: folder-where-the-apk uploades-to/apk-name 
    // and  folder-where-the-apk uploades-to/apk-name/dedexout
    
    def parseInApk(filePath: String) : (String, String) = {
      println("filePath" + filePath)
      val lst = filePath.split("/").toList
      val plen = lst.length
      println(plen)
      
      /*val fileName = lst(plen-1)
        println(fileName)
      val lst2 = fileName.split("\\.").toList
      val plen2 = lst2.length
      val fileFoldnerName = lst2(plen2-2) */
      
      val fileFoldnerName = filePath.split("\\.apk").toList.head
      
      
      println("Project Name to Analyzer::::::::::::: "+ fileFoldnerName)
      
      val pathToScript  = lst.dropRight(1).foldLeft("")((res, s) => {res + s + "/"})
      
     val getIRCmdStr = "/usr/bin/python ./getIR.py" + " " + pathToScript
      val res = runSysCmd(getIRCmdStr) 
      var projFolder = "";
      var  irfolder="";
        
      var continue = true
       while(continue) {
        if(res == 1){ // finished 
          continue = false
            projFolder = fileFoldnerName //"." + File.separator + "testapks" + File.separator + fileFoldnerName
           	 irfolder = fileFoldnerName + File.separator + "dedexout"//"." + File.separator + "testapks" + File.separator + fileFoldnerName + File.separator+"dedexout"
           
        } else if(res == 0) {
          println("timeed out. killed")
          
        }
      }
      // if should succeed
      if(!projFolder.isEmpty() && !irfolder.isEmpty()){
        	println("irFOLEDER--------" + irfolder)
        	println("projFOLDER-------" + projFolder)
           	(irfolder, projFolder)
      }
      else 
        ("", "")
     
    }
 
    def main(args: Array[String]) :Unit = {
        val curDir = System.getProperty("user.dir")
        
      //  val scriptDir = curDir + File.separator + "getIR.py" 
      //  val apkDir = curDir + File.separator + "testapks"
        
       // val pythoncmd = "/usr/bin/python"
          
      //  val finalCmd = pythoncmd + " " + scriptDir + " " +   apkDir
       // println("=====" + finalCmd)
       // runSysCmd(finalCmd)
       // parseInApk("./testapks/UltraCoolMap.apk")
        //runSysCmd("/usr/bin/python ./getIR.py")
            import sys.process._
        val str = "dot -Tsvg /Users/shuying/Documents/bk/wk_if/PlayPushDownOO/public/apks/jpgnetnoloop1660895081/jpgnetnoloop/graphs/graph-1-pdcfa-gc-lra.dot -o /Users/shuying/Documents/bk/wk_if/PlayPushDownOO/public/apks/jpgnetnoloop1660895081/jpgnetnoloop/graphs/graph-1-pdcfa-gc-lra.svg"
        
          str !
        
        
        System.exit(0)
    }
}