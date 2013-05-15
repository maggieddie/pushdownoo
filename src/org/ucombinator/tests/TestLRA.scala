package org.ucombinator.tests
import org.ucombinator.dalvik.preanalysis.LiveRegisterAnalysis
import org.ucombinator.dalvik.syntax.DalvikClassDef
import org.ucombinator.dalvik.parsing.S2DParser
import scala.tools.nsc.io.Directory
import java.io.File
import org.ucombinator.dalvik.syntax.SExp
import org.ucombinator.utils.CommonUtils
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.playhelpers.AnalysisHelperThread

import sys.process._

object TestLRA extends LiveRegisterAnalysis{

  
   private  def simplefunc(pf: tools.nsc.io.File) {
   
      val fp = pf.path// path.getAbsolutePath();
      val sexp = SExp.parseAllIn(fp)

     

      S2DParser(sexp);

     

    }
  private def parseDalvikSExprs() {
    val dirName =  "tests/sexps_smartcam"
    
    val sexDir  = new Directory(new File(dirName))
    val allFileList = sexDir.deepFiles
   
    allFileList.foreach(simplefunc)

  }
  
   def main(args: Array[String]): Unit = {
    
   /*    parseDalvikSExprs
       
      val onstart = DalvikClassDef.lookupMethod("com/smartcam/webcam/ui/SmartCamActivity$1","com/smartcam/webcam/ui/SmartCamActivity$1/onClick",List("(object android/content/DialogInterface)", "int"), 1)
      val meth = onstart.head
      
      val lst = CommonUtils.flattenLinkedStmt(List())(meth.body)
      println("the methoboday is right?")
      lst.foreach(println)
      runLRAOnListSts(lst)
        Thread.currentThread().asInstanceOf[AnalysisHelperThread].liveMap.foreach(println)*/
      
      //  val graphZipCmd : String = "cd " +  "./public/apks/jpgnetnoloop-2065195456/jpgnetnoloop/graphs/" + " && " + "tar -zcvf graph.tar.gz ./* "
       // val graphZipCmd : String = //"cd " +  "./public/apks/jpgnetnoloop-2065195456/jpgnetnoloop/graphs" +  " && " +  
      // "tar -zcvf graph.tar.gz public/apks/jpgnetnoloop-2065195456/jpgnetnoloop/graphs/* "
     
     //val pycmd =   "/usr/bin/python ./pytar.py" + " " + "./public/apks/jpgnetnoloop-2065195456/jpgnetnoloop/graphs"
     // pycmd !
      
       val pycmd2 =   "/usr/bin/python ./pytar.py" + " " + "./public/apks/jpgnetnoloop1425510450/jpgnetnoloop" + " all.tar.gz"
      pycmd2 !
      
      println("finished")
      
     
      
  }
}