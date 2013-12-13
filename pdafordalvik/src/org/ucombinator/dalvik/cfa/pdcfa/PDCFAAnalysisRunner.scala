/*
 * CRAPL 2012.
 * U Combinator, University of Utah
 * DistriNet, KU Leuven
 *
 * THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
 * APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
 * HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT
 * WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND
 * PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE
 * DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
 * CORRECTION.
 *
 * IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
 * WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR
 * CONVEYS THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
 * INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT
 * NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR
 * LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM
 * TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER
 * PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * If you have questions or concerns about the CRAPL, or you need more
 * information about this license, please contact:
 *
 *    Matthew Might
 *    http://matt.might.net/
 */

package org.ucombinator.dalvik.cfa.pdcfa

import org.ucombinator.utils.{ AnalysisType, AIOptions, FancyOutput }
import org.ucombinator.dalvik.syntax._
import org.ucombinator.dsg.DyckStateGraphMachinery
import org.ucombinator.dalvik.cfa.cesk.DalvikCFARunner
import org.ucombinator.dsg.DSGAnalysisRunner
import org.ucombinator.dalvik.cfa.cesk.CFAStatistics
import org.ucombinator.utils.CommonUtils
import org.ucombinator.utils.Debug
import org.ucombinator.utils.ParsingUtils
import org.ucombinator.dalvik.statistics.DalvikAnalysisStatistics
import org.ucombinator.dalvik.preanalysis.LiveRegisterAnalysis
import scala.collection.immutable.{ Set => ImmSet, Map => ImmMap }
import org.ucombinator.utils.StringUtils
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.dalvik.informationflow.DalInformationFlow
import scala.tools.nsc.io.Directory
import scala.util.matching.Regex
import org.ucombinator.dalvik.cfa.widening.DalvikWideningConfiguration
import sys.process._
import java.io.File
import java.io.FileWriter
import org.ucombinator.domains.GodelDomains
import org.ucombinator.domains.StandardDomains
import org.ucombinator.domains.CommonAbstractDomains.Store
import org.ucombinator.domains.CommonAbstractDomains.Value
import org.ucombinator.utils.NonNullUtils
import org.ucombinator.domains.CommonAbstractDomains.IntentExtraKeyTypeAndValue
//import org.ucombinator.utils.NonNullCheckUtils

class PDCFAAnalysisRunner(opts: AIOptions) extends DalvikCFARunner(opts)
  with StackCESKMachinary
  with PDCFAGarbageCollector
  with IPDSMachinery
  with DyckStateGraphMachinery
  with DSGAnalysisRunner
  with FancyOutput
  with DalvikAnalysisStatistics
  with LiveRegisterAnalysis
  with DalvikWideningConfiguration {
  //with NonNullCheckUtils {

  type Term = Stmt

  //don't need this 
  //type Value = Value

  override type Kont = List[Frame]

  // OK.stil not sure about the switch frames
  // but the javascript PDCFA runner will set to be true
  def canHaveSwitchFrames = false

  // neither is the store senseitive
  def isStoreSensitive(s: ControlState) = true

  def step(q: ControlState, k: Kont, frames: Kont, store: SharedStore, pStore: PSharedStore) = {
    val result = stepIPDS(q, k, frames)
    result.map {
      case (x, y) => (x, y, store, pStore)
    }
  }

  /**
   * TODO: but seems no need for now.
   */

  /* def alloc(v: Var, c: Conf): Addr = c match {
    case (PState(e, _, _, _), _) =>
      if (isDummy) {
        (SName("SingleAddr", 0), Nil)
      } else k match {
        case 0 => (v, Nil)
        case 1 => (v, List(e))
        case _ => throw new StackCESKException("Analysis not implemented for k greater than 1 (" + k + ")")
      }
    case _ => {
      throw new StackCESKException("Illegal allocation configuartion:\n" + c.toString)
    }
  }*/

  def computePointsToStaticsForAllDsgs(dsgs: List[DSG]): (VarPointsTo, ThrowPointsTo) = {

    var varPointsToList = List[VarPointsTo]()
    var throwPointsToList = List[ThrowPointsTo]()

    dsgs.foreach((dsg) => {

      val (varPointsToOne, throwPointsToOne) = computePointsToStatistics(dsg.nodes)
      varPointsToList = varPointsToOne :: varPointsToList
      throwPointsToList = throwPointsToOne :: throwPointsToList
    })

    var totalPointTp = 0
    var totalPtCardi = 0

    var totalThrows = 0
    var totalTc = 0

    varPointsToList.foreach((ve: VarPointsTo) => {
      totalPointTp += ve.totalEntries
      totalPtCardi += ve.totalCardi
    })

    throwPointsToList.foreach((tpt: ThrowPointsTo) => {
      totalThrows += tpt.totalEntries
      totalTc += tpt.totalCardi
    })

    (VarPointsTo(totalPointTp, totalPtCardi), ThrowPointsTo(totalThrows, totalTc))
  }

  private def incTime(explored: Int) {
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges = Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges + explored
  }

  private def appendSecurityText(buffer: StringBuffer,
    srcOrSinkSt: Boolean,
    taintSt: Boolean,
    matchRegex: Boolean,
    stO: Option[StForEqual],
    state: S,
    regex: Regex,
    entryStmt: Stmt): StringBuffer = {

    if (entryStmt != StmtNil) {
      buffer.append("<tr ><td bgcolor=")

      buffer.append("#FFF6FA")
      buffer.append(">")
      stO match {
        case Some(stq) => {
          buffer.append(stq.oldStyleSt)
        }
        case None => {
          buffer.append("None")
        }
      }

      buffer.append("</td> <td >")
      buffer.append("Trigger(entry-points)")
      buffer.append(" </td> </tr>")
    }

    if (srcOrSinkSt) {
      buffer.append("<tr ><td bgcolor=")

      buffer.append("#FFB2D8")
      buffer.append(">")
      stO match {
        case Some(stq) => {
          buffer.append(stq.oldStyleSt)
        }
        case None => {
          buffer.append("None")
        }
      }

      buffer.append("</td> <td >")
      buffer.append(state.taintKind)
      buffer.append(" </td> </tr>")
    } else if (taintSt) {
      buffer.append("<tr ><td bgcolor=")

      buffer.append("#FFCCE5")
      buffer.append(">")
      stO match {
        case Some(stq) => {
          buffer.append(stq.oldStyleSt)
        }
        case None => {
          buffer.append("None")
        }
      }
    } else if (matchRegex) {
      buffer.append("<tr ><td bgcolor=")

      buffer.append("#FFC3E1")
      buffer.append(">")
      stO match {
        case Some(stq) => {
          buffer.append(stq.oldStyleSt)
        }
        case None => {
          buffer.append("None")
        }
      }

      buffer.append("</td> <td >")
      buffer.append("Regex Matched")
      buffer.append(regex.toString)
      buffer.append(" </td> </tr>")
    }
    buffer
  }

  private def getEntryPointStmt(state: S): Stmt = {
    state match {
      case ps @ PartialState(st, fp, s, pst, kptr, t) => {
        st match {
          case (stq @ StForEqual(eS @ EntryPointInvokeStmt(en, objRegStr, nxt, ls, clsP, methP), nxss, lss, clsPP, methPP)) => {
            eS
          }
          case _ => StmtNil
        }

      }
      case _ => StmtNil
    }
  }

  def sortingRankings(rawNodes: List[S]): List[S] = {
    val rawNodes0 = rawNodes.filter((rn) => {
      rn match {
        case ErrorState(_, _, _) | FinalState(_) => {
          false
        }
        case _ => true
      }
    })
    val res = rawNodes0.sortBy {
      case ps @ PartialState(StForEqual(stmt, nxss, lss, clsP, methP), curFP, store, pst, kptr, t) => {
        -stmt.riskRanking
      }
    }
    // only preserve ranks > 0/// will it be expensive??
    res.filter {
      case ps @ PartialState(StForEqual(stmt, nxss, lss, clsP, methP), curFP, store, pst, kptr, t) => {
        stmt.riskRanking > 0
      }
    }
  }

  // flat is just whether or not to flatten all the nodes in dsgs.
  def getSortedRankedStates(dsgs: List[DSG], flat: Boolean): List[S] = {
    if (flat) {
      val rawNodes =
        dsgs.foldLeft(List[S]())((res, dsg) => {
          res ++ dsg.nodes
        })
      sortingRankings(rawNodes)
    } else { //TODO
      List()
    }
  }

  /**
   * *
   * For NonNUllability
   *
   */

  /**
   * proper fields: defined as espcially private object type, that is supposed to be initialized in the class
   * what about currently all declared type? NOTE: It should be consistent with the initObjec
   * currently, the initObject is initiazed non-static fields to be empty set.
   *
   */
  private def getProperFields(fieldDefs: List[FieldDef]): List[String] = {

    fieldDefs.foldLeft(List[String]())((res, fd) => {
      val ft = fd.ft
      val attrs = fd.atrs
      val strO = StringUtils.getTypeFromObjectWrapper(ft)
      if (strO != ft //&& !attrs.contains("static")
      ) { // we found something the actual object type string, {
        fd.fieldPath :: res
      } else res

    })
  }
  /**
   * computeNonNull helper: to explore the store and find field offset with values of nonnull
   * nonnull is judeged based on empty set or not.
   */
  private def computeCountForCurrentClass(clsName: String, clsDef: DalvikClassDef, store: Store): Int = {
    val dataMap = store.getMap
    val curFields = clsDef.fields

    val allNonNullFieldsCountsForCurrentClass =
      dataMap.map {
        case (k, v) => {
          val avs = v.toList
          val avsWithObjVs = avs.toSet.filter(_.isInstanceOf[ObjectValue])
          val avsWithObjVs2 = avsWithObjVs.map(_.asInstanceOf[ObjectValue])
          val avsWithObjVs3 = avsWithObjVs2.filter(_.clsName == clsName)
          val countsForTheSameClass: Set[Int] = avsWithObjVs3.map {
            av =>
              {
                val avov = av.asInstanceOf[ObjectValue]
                val op = avov.op
                val curClsName = avov.className
                if (curClsName == clsName) {
                  val objFieldOffSets = getProperFields(curFields).map(op.offset(_))
                  val objFieldVals = objFieldOffSets.map((fa) => {
                    storeLookup(store, fa)
                  })
                  objFieldVals.filter(!_.isEmpty).size
                } else 0
              }
          }
          if (!countsForTheSameClass.isEmpty)
            countsForTheSameClass.max
          else {
            0
          }
        }
      }

    if (!allNonNullFieldsCountsForCurrentClass.isEmpty)
      allNonNullFieldsCountsForCurrentClass.max
    else {
      //println("allNonNullfielddsCountForCurrentClass is empty1!!")
      0
    }
  }

  private def percent(num1: Int, num2: Int): Double = (num1.toDouble / num2.toDouble) * 100

  /**
   * after constructors,
   */

  def computeNonNull(monoStore: Store): Map[String, (Int, Int)] = {

    Thread.currentThread().asInstanceOf[AnalysisHelperThread].classTable.foldLeft(Map[String, (Int, Int)]())((res, clsDefEntry) => {
      val clsName = clsDefEntry._1
      val clsDef: DalvikClassDef = clsDefEntry._2

      var nonNullFieldCount = computeCountForCurrentClass(clsName, clsDef, monoStore)
      println(nonNullFieldCount)
      res + (clsName -> (clsDef.getDirectObjFields.size, nonNullFieldCount))
    })
  }

  private def fieldAddrsWithoutDupFieldCount(fas: Set[FieldAddr]): Int = {

    val resMap =
      fas.foldLeft(Map[String, FieldAddr]())((res, fs) => {
        res + (fs.offset -> fs)
      })

    resMap.size
  }

  // the return type is just to be consistent with the computNonNull
  def easyComputeNonNullAfterLinkedConstr(curClsName: String, monoStore: Store, directFields: List[String]): Map[String, (Int, Int)] = {
    // any fieldaddress of with current class name in the OP?
    println("curClass: " + curClsName + " the nonstatic fields")
    directFields.foreach(println)
    val fieldOffsetAddrsWithcurClsNameInOp = monoStore.getMap.foldLeft(List[FieldAddr]())((res, kv) => {
      val fa = kv._1
      val vals = kv._2

      fa match {
        case fai @ FieldAddr(op, fieldPath) => {
          //  println("the field Path", fieldPath) 
          if (op.clsName == curClsName && directFields.contains(fieldPath)) {
            // println("fieldPath passed: ", fieldPath)
            fai :: res
          } else res
        }
        case _ => res
      }
    })
    // look up then in the store to count how many of them are nonnull object typw
    val fieldOffsetAddrsWithObjVals = fieldOffsetAddrsWithcurClsNameInOp.filter((fa) => {
      val vals = monoStore.getOrElse(fa)
      //   println("vals: ")   
      // vals.toSet.foreach(println)

      val fvs = filterAbsObjAndStringTop(vals.toSet)

      // println("after filter: " + fvs.size)
      // fvs.foreach(println)
      !vals.isEmpty && !fvs.isEmpty //filterAbsObjValues(vals.toSet).isEmpty
    })

    println("clsName: fieldoffsetwithobjs: " + curClsName)
    println("nonnull computed: " + fieldOffsetAddrsWithObjVals.size + fieldOffsetAddrsWithObjVals)

    val resCount = fieldAddrsWithoutDupFieldCount(fieldOffsetAddrsWithObjVals.toSet)
    println("final count is ", resCount)
    if (directFields.size > 0) {
      Map(curClsName -> (directFields.size, resCount))
    } else Map()
  }

  /**
   * dump the statistics.
   */

  def dumpNonNullStatistic(opts: AIOptions, nonNullMap: Map[String, (Int, Int)], avgNonNullMap: Map[String, Double]): String = {
    import java.io._

    val buffer = new StringBuffer()
    var avg = 0.0
    if (opts.unlinkedNonNull) {
      avgNonNullMap.foreach {
        case (clsName, percentage) => {

          buffer.append(clsName + ": " + percentage)
          buffer.append("\n")
          avg = avg + percentage
        }
      }
      buffer.append("The total average: " + avg / avgNonNullMap.size)
    } else {
      nonNullMap.foreach {
        case (clsName, (total, nonNullCount)) => {
          val per = percent(nonNullCount, total)
          buffer.append(clsName + " " + "(" + nonNullCount + "/" + total + "=" + per + ")")
          buffer.append("\n")
          avg += per
        }
      }
      buffer.append("the total average: " + avg / nonNullMap.size)

    }

    buffer.append("\n Any places of null ref during constructions? \n ")

    // for null ref during construction
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].nullRefMap.foreach {
      case (st, cnt) => {
        if (cnt > 0)
          buffer.append(st.clsPath + ":" + st.methPath + st.lineNumber + "\n" + st + "\n")
      }
    }

    val stasticsDir = opts.statsDirName //opts.apkProjDir + File.separator + statisticsDirName

    val statDir = new Directory(new File(stasticsDir))
    if (!statDir.exists) {
      statDir.createDirectory(force = true)
      statDir.createFile(failIfExists = false)
    }

    /* val subfolderPath = statisticsDirName + File.separator + StringUtils.trimFileName(opts.sexprDir)
      val subfolder = new Directory(new File(subfolderPath))
      if (!subfolder.exists) {
        subfolder.createDirectory(force = true)
        subfolder.createFile(failIfExists = false)
      }*/
    val path = stasticsDir + File.separator + CommonUtils.getDumpFileName(opts, "notnull-") // or use opts.statsFilePath
    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)

    writer.write(buffer.toString)
    writer.close()

    println("Not null statistics dumped into: " + path)

    path

  }

 
  def dumpForIntentInput(opts: AIOptions): String = {
    import java.io._

    val buffer = new StringBuffer()

    val summarizeMap =
      Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap.foldLeft(Map[String, Map[String, Set[IntentExtraKeyTypeAndValue]]]())((res, kv) => {
        val (clsName, entryPoint, stmt) = kv._1
        val fieldMap = kv._2
        val newKey = clsName + "/" + entryPoint
        if (res.contains(clsName + "/" + entryPoint)) {
          val newMap = fieldMap ++ res(newKey)
          res + (newKey -> newMap)
        } else {
          res + (newKey -> fieldMap)
        }
      })

    Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap.foreach {
      case (newKey, fieldMap) => {
        if (!fieldMap.isEmpty) {
          buffer.append("Method: ", newKey)
          buffer.append("\n")
          fieldMap.foreach {
            case (op, vals) => {
              if (op.contains("getExtras")) {
                buffer.append("---Op: " + op + "\n---" + "(Key Type, Key Values)\n")
                vals.foreach((v) => buffer.append("(" + v.keyType + ", " + v.keyVal + ")" + "\n")) //buffer.append(v + "\n" ))
                buffer.append("\n")
              } else {
                buffer.append("---Op: " + op + "\n---" + "(Return Type, Args Values)\n")
                vals.foreach((v) => buffer.append("(" + v.keyType + ", " + v.keyVal + ")" + "\n")) //buffer.append(v + "\n" ))
                buffer.append("\n")
              }
            }
          }
        }
      }
    }

    val stasticsDir = opts.statsDirName //opts.apkProjDir + File.separator + statisticsDirName

    val statDir = new Directory(new File(stasticsDir))
    if (!statDir.exists) {
      statDir.createDirectory(force = true)
      statDir.createFile(failIfExists = false)
    }

    val path = stasticsDir + File.separator + CommonUtils.getDumpFileName(opts, "forIntentFuzzer-") // or use opts.statsFilePath
    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)

    writer.write(buffer.toString)
    writer.close()

    println("Flow-insensive output for fuzzer dumped into: " + path)

    path

  }

  def dumpRiskRanking(opts: AIOptions, dsgs: List[DSG]) {
    def colorColumn(cnt: Int, buffer: StringBuffer, value: String, colorStr: String) {

      buffer.append("<td bgcolor=")

      buffer.append(colorStr)
      buffer.append(">")
      buffer.append(value)
      buffer.append("</td>")
    }

    val reportedStateNodes = getSortedRankedStates(dsgs, true)

    var buffer = new StringBuffer()

    //title
    buffer.append("<html> <head>  <center> <title> Risk ranking </title> </center> </head> <h2> Risk Ranking  </h2><body> <table>\n")
    buffer.append("<tr ><td bgcolor=")
    buffer.append("#FFF6FA")
    buffer.append(">")
    // headers
    buffer.append("<b> Risk Ranking </b>")
    buffer.append("</td> <td >")
    buffer.append("<b> Categories </b>")
    buffer.append("</td> <td >")

    buffer.append("<b> Class </b>")
    buffer.append("</td> <td >")
    // one column
    buffer.append("<b> Method </b>")
    buffer.append("</td> <td >")

    buffer.append("<b> Line Number </b>")
    buffer.append("</td> <td >")

    buffer.append("<b> Statement </b>")
    buffer.append(" </td> </tr>")
    buffer.append("</br>")

    var cnt = 0
    //Just internating color
    reportedStateNodes.foreach((stateNode) => {

      val (clsName, methName, lnName) = stateNode.getSourceLocation
      val stmtStr = stateNode.getStmtForEqual match {
        case Some(st) => st.oldStyleSt.toString
        case None => ""
      }

      val colorStr = if (cnt % 2 == 0) {
        "FFFFFF" //white
      } else "#E8E8E8" // grey

      buffer.append("<tr >")
      colorColumn(cnt, buffer, stateNode.getRiskRanking.toString, colorStr)
      colorColumn(cnt, buffer, StringUtils.getOneStringFromSetofString(stateNode.taintKind), colorStr)
      colorColumn(cnt, buffer, clsName, colorStr)
      colorColumn(cnt, buffer, methName, colorStr)
      colorColumn(cnt, buffer, lnName, colorStr)
      colorColumn(cnt, buffer, stmtStr, colorStr)
      buffer.append("</tr>")

      cnt = cnt + 1
    })
    buffer.append("</table></body></html>")

    // file
    val reportDirName = opts.permReportsDirName //opts.apkProjDir + File.separator + statisticsDirName 
    println("path is: ", reportDirName)
    val secuDir = new Directory(new File(reportDirName))
    if (!secuDir.exists) {
      secuDir.createDirectory(force = true)
      secuDir.createFile(failIfExists = false)
    }

    val path = opts.riskRankingReportPath //stasticsDir + File.separator + CommonUtils.getStatisticsDumpFileName(opts) // or use opts.statsFilePath

    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)

    writer.write(buffer.toString)
    writer.close()

    println("Risk Ranking report dumped to: " + path)
    path

  }

  def dumpSecurityReport(opts: AIOptions, dsgs: List[DSG]) {

    var buffer = new StringBuffer()

    buffer.append("<html> <head> <title> Security Report </title> </head> <h2> Security Report  </h2><body> <table>\n")
    buffer.append("<tr ><td bgcolor=")
    buffer.append("#FFF6FA")
    buffer.append(">")
    buffer.append("<b>Context format: Statement@@@ClassName$$Methodname::LineNumber</b>")
    buffer.append("</td> <td >")
    buffer.append("Property")
    buffer.append(" </td> </tr>")
    buffer.append("</br>")

    dsgs.foreach((dsg) => {
      val edges = dsg.edges
      for (Edge(s, g, s1) <- edges if s != s1) {
        val srcOrSinkSt = s.sourceOrSinkState
        val taintSt = s.taintedState
        var regexMatchS = false
        var regexMatchS1 = false
        if (opts.regex ne null) {
          regexMatchS = s.matchRegex(opts.regex)
          regexMatchS1 = s1.matchRegex(opts.regex)
        }

        val srcOrSinkSt1 = s1.sourceOrSinkState
        val taintSt1 = s1.taintedState

        val entryPointStmt = getEntryPointStmt(s)
        val entryPointStmt1 = getEntryPointStmt(s1)

        val stO = s.getCurSt
        val st1 = s1.getCurSt

        buffer = appendSecurityText(buffer,
          srcOrSinkSt,
          taintSt,
          regexMatchS,
          stO,
          s,
          opts.regex, entryPointStmt)
        buffer = appendSecurityText(buffer,
          srcOrSinkSt1,
          taintSt1,
          regexMatchS1,
          st1,
          s1,
          opts.regex, entryPointStmt1)

      }
    })

    val reportDirName = opts.permReportsDirName //opts.apkProjDir + File.separator + statisticsDirName

    buffer.append("</table></body></html>")
    val secuDir = new Directory(new File(reportDirName))
    if (!secuDir.exists) {
      secuDir.createDirectory(force = true)
      secuDir.createFile(failIfExists = false)
    }

    val path = opts.securityReportPath //stasticsDir + File.separator + CommonUtils.getStatisticsDumpFileName(opts) // or use opts.statsFilePath
    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)

    writer.write(buffer.toString)
    writer.close()

    println("Security dumped into: " + path)
    path

  }
  
  
  def dumpDataFlowForFuzzer(opts: AIOptions, dsgs: List[DSG]) {

    var buffer = new StringBuffer()

    buffer.append("<html> <head> <title> Data Flow Information </title> </head> <h2> Data Flow Information  </h2><body>  \n")
    
    buffer.append("</br>")

    dsgs.foreach((dsg) => {
      buffer.append("")
      val edges = dsg.edges
      for (Edge(s, g, s1) <- edges if s != s1) {
        val srcOrSinkSt = s.sourceOrSinkState
        val taintSt = s.taintedState
        var regexMatchS = false
        var regexMatchS1 = false
        if (opts.regex ne null) {
          regexMatchS = s.matchRegex(opts.regex)
          regexMatchS1 = s1.matchRegex(opts.regex)
        }

        val srcOrSinkSt1 = s1.sourceOrSinkState
        val taintSt1 = s1.taintedState

        val entryPointStmt = getEntryPointStmt(s)
        val entryPointStmt1 = getEntryPointStmt(s1)

        val stO = s.getCurSt
        val st1 = s1.getCurSt

        stO match {
            case Some(stq) => { 
              buffer.append(stq.oldStyleSt)
            }
            case None => {
              buffer.append("None")
            }
          }
          buffer.append("</br>") 
          st1 match {
            case Some(stq) => { 
              buffer.append(stq.oldStyleSt.methPath)
            }
            case None => {
              buffer.append("None")
            }
          }
          buffer.append("</br>") 
      } 
      buffer.append("ONe DSG end")
      buffer.append("</br>")
      buffer.append("</br>")
    }) 

     buffer.append(" </body></html>")
    val stasticsDir = opts.statsDirName //opts.apkProjDir + File.separator + statisticsDirName

    val statDir = new Directory(new File(stasticsDir))
    if (!statDir.exists) {
      statDir.createDirectory(force = true)
      statDir.createFile(failIfExists = false)
    }
  
    val path = stasticsDir + File.separator + CommonUtils.getDumpFileName2(opts, "cfg-in-text-", "html") // or use opts.statsFilePath
    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)

    writer.write(buffer.toString)
    writer.close()

    println(" data flow information  dumped into: " + path)

    path

  }

  /**
   * Run Pushdown Control Flow Analysis
   * @param opts analysis options
   * @param list of linked lsit of initentrypoint -> entry-statement->entrypoints
   */
  def runPDCFA(opts: AIOptions, entryStmts: List[Stmt]) {

    // import org.ucombinator.domains.CommonAbstractDomains._

    var (inheritedStore: Store, inheritedPStore: Store) =
      if (opts.godel) {
        (GodelDomains.botStore, GodelDomains.botStore)
      } else {
        (StandardDomains.botStore, StandardDomains.botStore)
      }

    // var inheirtedStore: SharedStore = Map.empty
    // var inheirtedPStore: PSharedStore = Map.empty

    var exploredcCnt = 0
    var nonNullMap = Map[String, (Int, Int)]()
    var avgNonNullMap = Map[String, Double]()

    val firstTime = (new java.util.Date()).getTime
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].curThreadStartTime = firstTime
    // val firstHelper = new NewDSGHelper
    //var toStopEntryExploration = false
    val dsgs =
      if (!opts.doNotNullCheck) { // normal 
        println("All the entry points starts : *****************", entryStmts.length)
        entryStmts.foreach((entryStmt) =>
          CommonUtils.flattenLinkedStmt(List())(entryStmt).foreach(println))

        println("All the entry points ends*****************")

        // val firstHelper = new NewDSGHelper
        var toStopEntryExploration = false
        entryStmts.foldLeft(List[DSG]())((res, entryStmt) => {
          val firstHelper = new NewDSGHelper
          println("--------- explore the entry--------" + entryStmt.next)
          //CommonUtils.flattenLinkedStmt(List())(entryStmt).foreach(println)

          val (resultDSG, storee, pstoree, toStop) = evaluateDSG(entryStmt, entryStmt.methPath, inheritedStore, inheritedPStore, firstHelper)

          if (!toStopEntryExploration && toStop == "stop") toStopEntryExploration = true
          //incNoEdges(resultDSG.edges.size)
          //incNoStates(resultDSG.nodes.size) 
          println("[" + StringUtils.trimFileName(opts.sexprDir) + ", " + " " + Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfStates + "  " + Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges + " \n")

          inheritedPStore = storee
          inheritedPStore = pstoree

          if (!toStopEntryExploration) {
            exploredcCnt += 1
          }
          resultDSG :: res
        })
      } /** Unlike entry point saturation, we will run init methods class by class
   * and collect correspondent statistics 
   */ else { 
        if (opts.unlinkedNonNull) {
          var toStopEntryExploration = false
          //  val firstHelper = new NewDSGHelper
          println(">>>>> Doing Non-nullability  Checking UNLINKED(mainly for Android cores)")
          Thread.currentThread().asInstanceOf[AnalysisHelperThread].classTable.foldLeft(List[DSG]())((res, clsDefEntry) => {
            //val firstHelper = new NewDSGHelper
            var listOfNonNullPercentage = List[Double]()

            val clsName = clsDefEntry._1
            val clsDef: DalvikClassDef = clsDefEntry._2

            val allInitsForCurCls = clsDef.unlinkedInitEntryPoints

            if (NonNullUtils.shouldNotAnalyze(clsDef)) {
              println("no analyzeing ", clsDef.className)
              res
            } else {
              val resAllInitsDSGs =

                allInitsForCurCls.foldLeft(List[DSG]())((res2, ai) => {
                  ai match {
                    case entryStmt @ InitEntryPointStmt(_, _, _, _, _, _, _, _) => {
                      val firstHelper = new NewDSGHelper

                      var (inheritedStore0: Store, inheritedPStore0: Store) =
                        if (opts.godel) {
                          (GodelDomains.botStore, GodelDomains.botStore)
                        } else {
                          (StandardDomains.botStore, StandardDomains.botStore)
                        }

                      //val entryStmt = ai.asInstanceOf[InitEntryPointStmt]
                      println("explore init:::: entryStmt", entryStmt)
                      val (resultDSG, storee, pstoree, toStop) = evaluateDSG(entryStmt, entryStmt.methPath, inheritedStore0, inheritedPStore0, firstHelper)
                      if (!toStopEntryExploration && toStop == "stop") toStopEntryExploration = true

                      println(StringUtils.trimFileName(opts.sexprDir) + ", " + " " + Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfStates + "  " + Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges + " \n")

                      //inheritedPStore = storee
                      // inheritedPStore = pstoree

                      if (!toStopEntryExploration) {
                        exploredcCnt += 1
                      }

                      // monovariant stores after linked construcotds's exploration
                      val monoStore = getMonovariantStore(resultDSG.nodes, storee)
                      //println("the store in one class", monoStore)

                      // compute for the current one
                      val curClsSingleMap = easyComputeNonNullAfterLinkedConstr(clsName, monoStore, clsDef.getDirectObjFields)
                      val (total, nonCnt) = curClsSingleMap(clsName)
                      listOfNonNullPercentage = percent(nonCnt, total) :: listOfNonNullPercentage

                      resultDSG :: res2
                    }
                    case StmtNil => res2

                  }
                }) // inits separately explored.
              //computet the average

              val avgNonNullPercentage = listOfNonNullPercentage.sum / listOfNonNullPercentage.length
              if (listOfNonNullPercentage.length > 0) {
                avgNonNullMap += (clsName -> avgNonNullPercentage)
              }
              resAllInitsDSGs ::: res
            }
          })

        } else {
          if (opts.verbose) {
            println(">>>>> Doing Non-nullability Checking LINKED(mainly for Android cores)")
          }

          var toStopEntryExploration = false

          Thread.currentThread().asInstanceOf[AnalysisHelperThread].classTable.foldLeft(List[DSG]())((res, clsDefEntry) => {
            val clsName = clsDefEntry._1
            val clsDef: DalvikClassDef = clsDefEntry._2
            // each exploration will have new helper
            val firstHelper = new NewDSGHelper

            if (NonNullUtils.shouldNotAnalyze(clsDef)) {
              if (opts.verbose)
                println("no analyzeing ", clsDef.className)
              res
            } else {
              clsDef.linkedInitEntryStmt match {
                case Some(initEntry) => {

                  val entryStmt = initEntry.asInstanceOf[InitEntryPointStmt]
                  if (opts.verbose)
                    println("explore init:::: entryStmt", entryStmt)
                  val (resultDSG, storee, pstoree, toStop) = evaluateDSG(entryStmt, entryStmt.methPath, inheritedStore, inheritedPStore, firstHelper)
                  if (!toStopEntryExploration && toStop == "stop") toStopEntryExploration = true

                  println(StringUtils.trimFileName(opts.sexprDir) + ", " + " " + Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfStates + "  " + Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges + " \n")

                  inheritedPStore = storee
                  inheritedPStore = pstoree

                  if (!toStopEntryExploration) {
                    exploredcCnt += 1
                  }

                  // monovariant stores after linked construcotds's exploration
                  val monoStore = getMonovariantStore(resultDSG.nodes, storee)
                  //  println("the store in one class", monoStore)

                  // compute for the current one
                  nonNullMap ++= easyComputeNonNullAfterLinkedConstr(clsName, monoStore, clsDef.getDirectObjFields) //computeNonNull(monoStore)

                  resultDSG :: res
                }
                case None => res
              }
            }
          })
        }
      }
     val secondTime = (new java.util.Date()).getTime
    val delta = secondTime - firstTime

    if (opts.doNotNullCheck) {
      dumpNonNullStatistic(opts, nonNullMap, avgNonNullMap)
    }

    if (opts.forIntentFuzzer) {
    //  dumpForIntentInput(opts)
       val path = getGraphParentFolder(opts)
      dumpPathsWithIntentsRelated(opts, dsgs)
     // dumpDataFlowForFuzzer(opts,dsgs)
    }

    if (opts.verbose) {
       println()
    println("The analysis has taken " + (
      if (delta / 1000 < 1) "less than one second."
      else if (delta / 1000 == 1) "1 second."
      else delta / 1000 + " seconds."))
      println("Pushdown analysis finished.")
    }

    if (!opts.forIntentFuzzer && !opts.doNotNullCheck) {
      dumpSecurityReport(opts, dsgs)
    }

    if (opts.printPaths) {
      val path = getGraphParentFolder(opts)
      dumpSimplePaths(opts, dsgs)
      println()
    }

    if (opts.dumpGraph) {
      val path = getGraphParentFolder(opts)

      val res = prettyPrintDSGs(dsgs, path, opts)
      println()
      if (!opts.simplifyGraph && res.contains("Final")) {
        if (opts.verbose) {
          println("Has final state.\n")
        }
      } else if (!opts.simplifyGraph) {
        println("Warning: no final state!\n")
      }
    }

    println()
    println("Computing statistics")
    println()

    val interrupted = opts.interrupt && Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges > opts.interruptAfter //resultDSG.nodes.size > opts.interruptAfter

    /*  dumpStatistics(opts, CFAStatistics(delta, //sizeExp, //allVars.size,
   //   singletons.size, 
      resultDSG.nodes.size, resultDSG.edges.size, interrupted))*/

    val (varPointsto, throwPointsto) = computePointsToStaticsForAllDsgs(dsgs) //computePointsToStatistics(resultDSG.nodes) // is this states?

    val analysisStatistics = AnalysisStatistics(
      delta,
      varPointsto,
      throwPointsto,
      Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfStates, Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges,
      interrupted,
      entryStmts.length,
      exploredcCnt)

    if (opts.verbose)
      println("explored entrypoints: " + exploredcCnt)

    dumpStatisticsNew(opts, analysisStatistics)

    if (opts.doNotNullCheck || opts.forIntentFuzzer) {
      DalInformationFlow.dumpPermReport(opts)
      dumpHeatMap(opts)
    }

    val reportTar = "/usr/bin/python ./pyreporttar.py" + " " + opts.permReportsDirName + " reports.tar.gz"

    reportTar !

    if (opts.dumpGraph) {
      println()
      println("Writing Dyck State Graph")
      println()
      val path = dumpDSGGraph(opts, dsgs)
      println("Dyck State Graph dumped into " + path)
    }

  }
}



