package org.ucombinator.dalvik.cfa.cesk

import scala.tools.nsc.io.Directory
import org.ucombinator.utils._
import java.io.File
import org.ucombinator.dalvik.syntax.SExp
import org.ucombinator.dalvik.parsing.S2DParser
import org.ucombinator.dalvik.syntax.{ Stmt, DalvikClassDef, MethodDef, InvokeStaticStmt, StmtNil, LineStmt }
import org.ucombinator.dalvik.vmrelated.DalvikVMRelated
import org.ucombinator.dalvik.vmrelated.APISpecs
import org.ucombinator.dalvik.statistics.DalvikAnalysisStatistics
import org.ucombinator.dalvik.syntax.NopStmt
import org.ucombinator.dalvik.statistics.Statistics
import org.ucombinator.playhelpers.AnalysisHelperThread
 

abstract class AnalysisRunner(opts: AIOptions) extends FancyOutput
  with DalvikVMRelated
  with DalvikAnalysisStatistics {

  type ControlState

  def k = opts.k

  // what's the isDummy?
  def isDummy = opts.dummy

  // when to use the simplify?
  def simplify = opts.simplifyGraph

  // know the reason to use the lazy val?
  lazy val isVerbose = opts.verbose

  // when will the progress Prefix be used
  lazy val progressPrefix = ("[" + StringUtils.trimFileName(opts.sexprDir) + ", "
    + getAnalysisKind(opts) + "]")

  def shouldGC = opts.gc

  def doLRA = opts.doLRA

  def dumpDSG = opts.dumpGraph

  def printGCDebug = opts.gcDebug

  // truncate the exploration of number  interruptAfter
  def interrupt = opts.interrupt

  def interruptAfter = opts.interruptAfter
  
  def timeInterrupt = opts.timeInterrupt
  
  def interruptAfterTime = opts.interruptAfterTime
  

  /**
   * Pretty-print analysis type
   */
  def getAnalysisKind(opts: AIOptions): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-CFA"
      case AnalysisType.PDCFA => "-PDCFA"
    }

    val analysis = if (opts.dummy) {
      "dummy"
    } else {
      opts.k
    }
    val withGC = if (opts.gc) "-gc" else ""
    analysis + cfa + withGC
  }
  
  def updateHeatMapPecentil {
     val curHeatMap = Thread.currentThread().asInstanceOf[AnalysisHelperThread].heatMap
     
     val totalPairs= curHeatMap.values
     var totalCount = 0
     
     totalPairs.foreach((hp) => {
       totalCount +=  hp.cnt 
     })
     
     curHeatMap.foreach({
       case (k, v) => {
         val cnt = v.cnt
         val percent = Math.ceil((cnt.toDouble / totalCount.toDouble) * 100)
         v.percentil = percent
       }
     })
     
  }
  
  private def getColor(p: Double) : String = {
    if(p >= 0 && p <1) {
      "#FFFFFF"
    }else if(p>=1 && p < 2) {
      "#FFF6FA"
    } else if(p>=2 && p<3){
      "FFEEF6"
    } else if(p>=3 && p < 4){
      "FFE5F2"
    } else if(p >=4 && p < 10) {
      "FFE4E1"
    } else if(p>=10 && p <15) {
      "FFCCE5"
    } else if(p >= 15 && p <20 ) {
    	"FFC3E1"
    } else if(p>=20 && p < 30) {
      "FFBBDD"
    } else if(p>=30 && p < 40) {
      "#FFB2D8"
    } else if (p >=40 && p <50) {
      "#FFAAD4"
    } else if (p >= 50 && p < 60) {
      "#FFA1D0"
    } else if (p>= 60 && p <70) {
      "#FF99CC"
    } else if(p >=70 && p< 80) {
       "#E68AB8"
    } else if(p >= 80 && p < 90) {
      "#B26B8F"
    } else if(p>=90 && p < 100) {
      "#995C7A"
    } else "#CC0064"
    
  }
  
 
  
  
  
  def dumpHeatMap(opts: AIOptions) {
    
    import java.io.File
    import java.io.FileWriter
    
    val curHeatMap = Thread.currentThread().asInstanceOf[AnalysisHelperThread].heatMap 
    // compute the percentage
     updateHeatMapPecentil
    
    val buffer = new StringBuffer()
    buffer.append("<html> <head> <title> Heat Map </title> </head> <h2> Abstract profiling: Heat Map </h2><body> <table>\n")
     
       buffer.append("<tr ><td bgcolor=")
      buffer.append("#FFF6FA")
        buffer.append(">")
          buffer.append("<b>Context format: Statement@@@ClassName$$Methodname::LineNumber</b>")
         buffer.append("</td> <td >")
         buffer.append("Precentile")
        buffer.append(" </td> </tr>")
        buffer.append("</br>")
        
        
    curHeatMap.foreach({
      case (k,v) => {
        buffer.append("<tr ><td bgcolor=")
        val p = v.percentil
        val color = getColor(p)
        buffer.append(color)
        buffer.append(">")
        buffer.append(k)
        buffer.append("</td> <td >")
        buffer.append(p)
        buffer.append(" </td> </tr>")
      }
    })
    
    buffer.append("</table></body></html>")
    
     val heatMapDirName = opts.permReportsDirName//opts.apkProjDir + File.separator + statisticsDirName
   
      val statDir = new Directory(new File(heatMapDirName))
      if (!statDir.exists) {
        statDir.createDirectory(force = true)
        statDir.createFile(failIfExists = false)
      }

    
      val path = opts.heatMapReportPath //stasticsDir + File.separator + CommonUtils.getStatisticsDumpFileName(opts) // or use opts.statsFilePath
      val file = new File(path)
      if (!file.exists()) {
        file.createNewFile()
      }
      val writer = new FileWriter(file)

      writer.write(buffer.toString)
      writer.close()

      println("HeatMap dumped into: " + path)

      path
    
    
  }

  def dumpStatisticsNew(opts: AIOptions, stat: AnalysisStatistics): String = {
    import java.io._
    val AnalysisStatistics(analysisTime, varPointsTo, throwPointsTo, numStates, numEdges, truncated) = stat
    val buffer = new StringBuffer()
    val meanRegular = (varPointsTo.totalCardi.toDouble / varPointsTo.totalEntries).toDouble
    val meanThrown = (throwPointsTo.totalCardi.toDouble / throwPointsTo.totalEntries).toDouble

   

    buffer.append("Control states: " + numStates + "\n")
    buffer.append("Transitions / DSG edges: " + numEdges + "\n")
    buffer.append("Total amount of VarPointsto entries, and the mean: " +
      varPointsTo.totalCardi + " | " + Math.ceil(meanRegular) + "\n")

       val (totalCardi, mean) = Statistics.totalAndMeanThrowPointsTo
    buffer.append("Total amount of ThrowPointsto entries, and the mean: " +
      totalCardi + " | " + mean + "\n")
      
    //  buffer.append("Total amount of ThrowPointsto entries, and the mean: " +
     // throwPointsTo.totalCardi + " | " + Math.ceil(meanThrown) + "\n")

    val (methCardies, meanObjs) = Statistics.totalAndMeanCallObjs
    buffer.append("Total amount of invoking objects, and the mean: " +
      // throwPointsTo.totalEntries + " | " + Math.ceil(meanThrown) + "\n")
      methCardies + " | " + Math.ceil(meanObjs) + "\n")

    val (cardiecs, meanec) = Statistics.totalAndAverageEclinks
    buffer.append(" E-C Links: " +
      // throwPointsTo.totalEntries + " | " + Math.ceil(meanThrown) + "\n")
      cardiecs + " | " + meanec + "\n")

    buffer.append("Analysis run for: " + analysisTime + " milliseconds\n")
    if (truncated) {
      buffer.append("Interrupted after " + opts.interruptAfter + " states.")
    }
   
    if (isVerbose) {
      println(buffer.toString)
    }

    val stasticsDir = opts.statsDirName//opts.apkProjDir + File.separator + statisticsDirName
    
      val statDir = new Directory(new File(stasticsDir))
      if (!statDir.exists) {
        statDir.createDirectory(force = true)
        statDir.createFile(failIfExists = false)
      

     /* val subfolderPath = statisticsDirName + File.separator + StringUtils.trimFileName(opts.sexprDir)
      val subfolder = new Directory(new File(subfolderPath))
      if (!subfolder.exists) {
        subfolder.createDirectory(force = true)
        subfolder.createFile(failIfExists = false)
      }*/
      val path = opts.statsPath //stasticsDir + File.separator + CommonUtils.getStatisticsDumpFileName(opts) // or use opts.statsFilePath
      val file = new File(path)
      if (!file.exists()) {
        file.createNewFile()
      }
      val writer = new FileWriter(file)

      writer.write(buffer.toString)
      writer.close()

      println("Statistics dumped into: " + path)

      path
    } else ""
  }

  /**
   * To be deleted
   */
  def dumpStatistics(opts: AIOptions, stat: CFAStatistics): String = {
    import java.io._

    val CFAStatistics(time, states, edges, interrupted) = stat

    val buffer = new StringBuffer()
    //buffer.append("Expressions: " + size + "\n")
    buffer.append("Control states: " + states + "\n")
    buffer.append("Transitions / DSG edges: " + edges + "\n")
    // buffer.append("Total amount of variables: " + vars + "\n")
    //  buffer.append("Singletons: " + singletons + "\n")
    buffer.append("Analysis run for: " + time + " milliseconds\n")
    if (interrupted) {
      buffer.append("Interrupted after " + opts.interruptAfter + " states.")
    }

    if (isVerbose) {
      println(buffer.toString)
    }

    if (opts.dumpStatistics) {
      val statDir = new Directory(new File(statisticsDirName))
      if (!statDir.exists) {
        statDir.createDirectory(force = true)
        statDir.createFile(failIfExists = false)
      }

      val subfolderPath = statisticsDirName + File.separator + StringUtils.trimFileName(opts.sexprDir)
      val subfolder = new Directory(new File(subfolderPath))
      if (!subfolder.exists) {
        subfolder.createDirectory(force = true)
        subfolder.createFile(failIfExists = false)
      }
      val path = subfolderPath + File.separator + CommonUtils.getStatisticsDumpFileName(opts)
      val file = new File(path)
      if (!file.exists()) {
        file.createNewFile()
      }
      val writer = new FileWriter(file)

      writer.write(buffer.toString)
      writer.close()

      println("Statistics dumped into: " + path)

      path
    } else ""

  }

  private def simplefunc(pf: tools.nsc.io.File, cnt: Int) {

    val fp = pf.path // path.getAbsolutePath();
    val sexp = SExp.parseAllIn(fp)

    if (opts.verbose) {
      // System.err.println("Input program in S-expression Form:")
      //  System.out.println(sexp)
      System.out.println("\n")
    } 

    S2DParser(sexp);

    if (opts.verbose) {
      System.err.println("done parsing file: " + fp)
    }

  }
  private def parseDalvikSExprs(opts: AIOptions) {
    val dirName = opts.sexprDir

    val sexDir = new Directory(new File(dirName))
    val allFileList = sexDir.deepFiles
    //val valSexFileList = allFileList.filter(_.name.endsWith(".sxddx"))
    // val arrFiles = new java.io.File(dirName).listFiles.filter(_.getName.endsWith(".sxddx"))
    //println("parse file length:", allFileList.length)

    val sexpFiles = allFileList.filter((f) => {
      f.name.endsWith(".sxddx")
    })

    var cnt = 0
    sexpFiles.foreach((sf) => {
      cnt += 1
      
    if (opts.verbose) {
      System.err.println(cnt + " Parsing file " + sf)
    }
      simplefunc(sf, cnt)
    })

    if (opts.verbose) {
      System.err.println(" Done passing all the s-exp class files! ")
      System.out.println("\n")
    }
  }
  
   
  def getListofInitEntries(opts: AIOptions) :  (List[Stmt], List[Stmt]) = {
    
      //parse in s-expressioned dalvik 
     parseDalvikSExprs(opts)
     
     //all the init-ens pathss
      getLinkedEntryPointHead(opts) 
  }
  
  
 /* *//**
   * it is gonna starting from the stmt returned 
   * the list of statements returned is for lra pass
   *//*
  def getAndroidEntry4Test(opts: AIOptions): (String, Stmt, List[Stmt]) = {
    *//**
     * parse in s-expressioned dalvik
     *//*
    parseDalvikSExprs(opts)
    //val entryMethodDef: List[MethodDef] = DalvikClassDef.lookupMethod("com/android/demo/notepad3/NoteEdit", "com/android/demo/notepad3/NoteEdit/factorial", List("int"), true)
    // val entryMethodDef: List[MethodDef] = 
    //   DalvikClassDef.lookupMethod("org/ucomb/tests/TestFieldsActivity", "org/ucomb/tests/TestFieldsActivity/onCreate", List("(object android/os/Bundle)"), true)
    // yeah, jsut factorial for testing.
    //  (entryMethodDef.head.methodPath, entryMethodDef.head.body)
    // Debug.prntDebugInfo(" the oncreate tentry",entryMethodDef.head.body)
    // linkClinitStmt(entryMethodDef.head.body)

    *//**
     * extract the entry point of the
     *//*
    val (en, allIndividualInits) = getLinkedEntryPointHead

    *//**
     * get all class constructors
     *//*
    //Debug.prntDebugInfo("entry point is", en)
    val (a, b) = stubClinit(en)
    //  Debug.prntDebugInfo("link", (a,b))
    (a, b, allIndividualInits)
  }*/

  def stubClinit(entryStmt: Stmt): (String, Stmt) = {

    /**
     * TODO: to fix the class path and methath
     */
    val clsPath = ""
    val methPath = ""
    val nopSt = NopStmt(StmtNil, StmtNil, clsPath, methPath)
    nopSt.next = entryStmt
    nopSt.lineNumber = LineStmt("01", StmtNil, StmtNil, clsPath, methPath)

    val allLists = CommonUtils.flattenLinkedStmt(List())(nopSt)

    println("sublinet")
    allLists.foreach(println)
    ("empty clint", nopSt)
  }

  /**
   * TODO: to fix the class path and methath
   */
  def linkClinitStmt(entryStmt: Stmt): (String, Stmt) = {
    val clinitPath = DalvikClassDef.getAllClInitPaths

    if (clinitPath.isEmpty) {
      (entryStmt.toString(), entryStmt)
    } else {
      val clsPath = ""
      val methPath = ""
      val invokingClInits = clinitPath.map(
        InvokeStaticStmt(_, List(), List(), StmtNil, LineStmt("-1", StmtNil, StmtNil, clsPath, methPath), clsPath, methPath))
      Debug.prntDebugInfo("before the inits", invokingClInits)

      val linkHeadO =
        CommonUtils.linkedListWrapper(List())(invokingClInits)
      val linkHead =
        linkHeadO match {
          case Some(s) => s
          case None => { StmtNil }
        }

      val flatlist = CommonUtils.flattenLinkedStmt(List())(linkHead)
      Debug.prntDebugInfo("after lnking clinit", flatlist.length)
      Debug.prntDebugInfo("the last stmt is always StmtNil?", flatlist.last)
      val secondLastStmt = flatlist.takeRight(2).head
      Debug.prntDebugInfo("the last second stmt is ", secondLastStmt)
      // link with the currnet entry
      secondLastStmt.next = entryStmt
      Debug.prntDebugInfo("the next is ", secondLastStmt.next)

      //Debug.prntDebugInfo("the next is ", secondLastStmt.next.next.next.next)*/
      (clinitPath.head, linkHead)
    }
  }

}