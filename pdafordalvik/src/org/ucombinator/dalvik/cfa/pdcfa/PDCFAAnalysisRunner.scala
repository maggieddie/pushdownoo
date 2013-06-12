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

import org.ucombinator.utils.{AnalysisType, AIOptions, FancyOutput}
import org.ucombinator.dalvik.syntax._
import org.ucombinator.dsg.DyckStateGraphMachinery
import org.ucombinator.dalvik.cfa.cesk.DalvikCFARunner
import org.ucombinator.dsg.DSGAnalysisRunner
import org.ucombinator.dalvik.cfa.cesk.CFAStatistics
import java.io.File
import org.ucombinator.utils.CommonUtils
import org.ucombinator.utils.Debug
import org.ucombinator.utils.ParsingUtils
import org.ucombinator.dalvik.statistics.DalvikAnalysisStatistics
import org.ucombinator.dalvik.preanalysis.LiveRegisterAnalysis
import scala.collection.immutable.{ Set => ImmSet, Map => ImmMap}
import org.ucombinator.utils.StringUtils
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.dalvik.informationflow.DalInformationFlow
import scala.tools.nsc.io.Directory
import scala.util.matching.Regex
import org.ucombinator.dalvik.cfa.widening.DalvikWideningConfiguration
import sys.process._


class PDCFAAnalysisRunner(opts: AIOptions) extends DalvikCFARunner(opts) 
										with StackCESKMachinary
										with PDCFAGarbageCollector 
										with IPDSMachinery 
										with DyckStateGraphMachinery 
										with DSGAnalysisRunner 
										with FancyOutput 
										with DalvikAnalysisStatistics 
										with LiveRegisterAnalysis 
										with DalvikWideningConfiguration{

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
  
  
   
  def computePointsToStaticsForAllDsgs(dsgs: List[DSG]) : (VarPointsTo, ThrowPointsTo) = {
     
    var varPointsToList = List[VarPointsTo]()
    var throwPointsToList = List[ThrowPointsTo]()
    
    dsgs.foreach((dsg) => {
      
       val (varPointsToOne, throwPointsToOne) = computePointsToStatistics(dsg.nodes)
       varPointsToList =    varPointsToOne :: varPointsToList  
       throwPointsToList = throwPointsToOne :: throwPointsToList 
    })
    
    var totalPointTp = 0
    var totalPtCardi = 0
    
    var totalThrows =0
    var totalTc = 0
    
    varPointsToList.foreach( (ve : VarPointsTo) => {
      totalPointTp += ve.totalEntries
      totalPtCardi += ve.totalCardi 
    })
    
    throwPointsToList.foreach( (tpt : ThrowPointsTo) => {
      totalThrows += tpt.totalEntries
      totalTc += tpt.totalCardi 
    })
    
   
    (VarPointsTo(totalPointTp, totalPtCardi), ThrowPointsTo(totalThrows, totalTc)) 
  }

  private def incNoEdges(explored: Int) {
   Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges = Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges + explored
 }
 
 
 
 private def incNoStates(explored:Int) {
   Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfStates += explored
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
		  						 entryStmt: Stmt) : StringBuffer = {
    
    if(entryStmt!=StmtNil){
      buffer.append("<tr ><td bgcolor=")
         
        buffer.append("#FFF6FA")
        buffer.append(">")
        stO match{
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
    
     if(srcOrSinkSt ){
           buffer.append("<tr ><td bgcolor=")
         
        buffer.append("#FFB2D8")
        buffer.append(">")
        stO match{
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
        } else if(taintSt) {
          buffer.append("<tr ><td bgcolor=")
         
        buffer.append("#FFCCE5")
        buffer.append(">")
        stO match{
             case Some(stq) => {
               buffer.append(stq.oldStyleSt)
             }
             case None => {
               buffer.append("None")
             }
           }
        }else if(matchRegex)  {
             buffer.append("<tr ><td bgcolor=")
         
        buffer.append("#FFC3E1")
        buffer.append(">")
        stO match{
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
  
private def getEntryPointStmt(state: S) : Stmt = {
   state match{
        case ps@PartialState(st, fp, s, pst, kptr, t) => {
        	st match {
        	  case (stq@StForEqual(eS @ EntryPointInvokeStmt(en, objRegStr, nxt, ls, clsP, methP), nxss, lss, clsPP, methPP)) => {
        	    eS
        	  }
        	  case _ => StmtNil
        	}
          
        }
        case _ => StmtNil
      } 
    }
 
 
 def dumpSecurityReport(opts: AIOptions, dsgs: List[DSG]) {
    import java.io.File
    import java.io.FileWriter
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
    
    dsgs.foreach( (dsg) => {
      val edges = dsg.edges
       for (Edge(s, g, s1) <- edges if s != s1) {
        val srcOrSinkSt = s.sourceOrSinkState
        val taintSt = s.taintedState 
        var regexMatchS = false
        var regexMatchS1 = false
        if(opts.regex ne null) {
        	regexMatchS = s.matchRegex(opts.regex)
        	regexMatchS1  = s1.matchRegex(opts.regex)
        }
        
        val srcOrSinkSt1 = s1.sourceOrSinkState
        val taintSt1 = s1.taintedState 
        
     
         
         val entryPointStmt = getEntryPointStmt(s)
        val entryPointStmt1 = getEntryPointStmt(s1)
        
        val stO = s.getCurSt
        val st1 = s1.getCurSt
        
       buffer =  appendSecurityText(buffer , 
		  						 srcOrSinkSt , 
		  						 taintSt , 
		  						 regexMatchS, 
		  						 stO, 
		  						 s , 
		  						 opts.regex, entryPointStmt)
		buffer =  appendSecurityText(buffer , 
		  						 srcOrSinkSt1 , 
		  						 taintSt1 , 
		  						 regexMatchS1, 
		  						 st1, 
		  						 s1 , 
		  						 opts.regex,entryPointStmt1)   
        
       }}) 
     val reportDirName = opts.permReportsDirName//opts.apkProjDir + File.separator + statisticsDirName
    
       buffer.append("</table></body></html>")
      val secuDir = new Directory(new File(reportDirName))
      if (!secuDir.exists) {
        secuDir.createDirectory(force = true)
        secuDir.createFile(failIfExists = false)
    
 
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
    } else ""
  }
  
 

 
  /**
   * Run Pushdown Control Flow Analysis
   * @param opts analysis options
   * @param list of linked lsit of initentrypoint -> entry-statement->entrypoints
   */
  def runPDCFA(opts: AIOptions, entryStmts: List[Stmt] ) { 
    
    var inheirtedStore: SharedStore = Map.empty
    var inheirtedPStore: PSharedStore = Map.empty
    
    var exploredcCnt = 0
    
     println("All the entry points starts*****************")
    entryStmts.foreach((entryStmt) => 
      CommonUtils.flattenLinkedStmt(List())(entryStmt).foreach(println) 
      )
    
    println("All the entry points ends*****************")
     val firstTime = (new java.util.Date()).getTime
     Thread.currentThread().asInstanceOf[AnalysisHelperThread].curThreadStartTime = firstTime
     
    val dsgs =
      entryStmts.foldLeft(List[DSG]())( (res, entryStmt) => {
       
        println("--------- explore the entry--------" + entryStmt.next)
        //CommonUtils.flattenLinkedStmt(List())(entryStmt).foreach(println)
    	val (resultDSG, storee, pstoree) = evaluateDSG(entryStmt, entryStmt.methPath, inheirtedStore, inheirtedPStore) 
    	
    	incNoEdges(resultDSG.edges.size)
    	incNoStates(resultDSG.nodes.size)
    	
    	 println("[" + StringUtils.trimFileName(opts.sexprDir) + ", " + " " + Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfStates  + "  " + Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges   + " \n")
    
    	inheirtedStore = storee
    	inheirtedPStore = pstoree 
    	exploredcCnt += 1
    	
    	resultDSG :: res
    }) 
    
    val secondTime = (new java.util.Date()).getTime
    val delta = secondTime - firstTime

    println()
    println("The analysis has taken " + (
      if (delta / 1000 < 1) "less than one second."
      else if (delta / 1000 == 1) "1 second."
      else delta / 1000 + " seconds."))

    if (opts.verbose) {
      println()
      println("Dyck State Graph computed.")
    }
    
     dumpSecurityReport(opts,dsgs)

    if (opts.verbose && opts.dumpGraph) {
      val path = getGraphParentFolder(opts)
      
      val res = prettyPrintDSGs(dsgs, path,opts)
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
    
    val (varPointsto, throwPointsto) = computePointsToStaticsForAllDsgs(dsgs)//computePointsToStatistics(resultDSG.nodes) // is this states?
 
    val analysisStatistics = AnalysisStatistics(
        delta,  
    	varPointsto, 
    	throwPointsto,   
    	Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfStates,  Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges, 
    	interrupted,
    	entryStmts.length, 
    	exploredcCnt)
    println("explored entrypoints: " + exploredcCnt )
    dumpStatisticsNew(opts, analysisStatistics) 
    DalInformationFlow.dumpPermReport(opts)
    dumpHeatMap(opts)
    
     val reportTar=   "/usr/bin/python ./pyreporttar.py" + " " + opts.permReportsDirName + " reports.tar.gz"
       
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



