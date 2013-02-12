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

   // val (methCardies, meanObjs) = Statistics.totalAndMeanCallObjs
   // buffer.append("Total amount of invoking objects, and the mean: " +
      // throwPointsTo.totalEntries + " | " + Math.ceil(meanThrown) + "\n")
     // methCardies + " | " + Math.ceil(meanObjs) + "\n")

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
      val path = subfolderPath + File.separator + getStatisticsDumpFileName(opts)
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
      val path = subfolderPath + File.separator + getStatisticsDumpFileName(opts)
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

  def getAndroidEntry4Test(opts: AIOptions): (String, Stmt, List[Stmt]) = {
    /**
     * parse in s-expressioned dalvik
     */
    parseDalvikSExprs(opts)
    //val entryMethodDef: List[MethodDef] = DalvikClassDef.lookupMethod("com/android/demo/notepad3/NoteEdit", "com/android/demo/notepad3/NoteEdit/factorial", List("int"), true)
    // val entryMethodDef: List[MethodDef] = 
    //   DalvikClassDef.lookupMethod("org/ucomb/tests/TestFieldsActivity", "org/ucomb/tests/TestFieldsActivity/onCreate", List("(object android/os/Bundle)"), true)
    // yeah, jsut factorial for testing.
    //  (entryMethodDef.head.methodPath, entryMethodDef.head.body)
    // Debug.prntDebugInfo(" the oncreate tentry",entryMethodDef.head.body)
    // linkClinitStmt(entryMethodDef.head.body)

    /**
     * extract the entry point of the
     */
    val (en, allIndividualInits) = getLinkedEntryPointHead

    /**
     * get all class constructors
     */
    //Debug.prntDebugInfo("entry point is", en)
    val (a, b) = stubClinit(en)
    //  Debug.prntDebugInfo("link", (a,b))
    (a, b, allIndividualInits)
  }

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