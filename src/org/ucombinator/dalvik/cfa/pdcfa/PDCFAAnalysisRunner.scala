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



class PDCFAAnalysisRunner(opts: AIOptions) extends DalvikCFARunner(opts) 
										with StackCESKMachinary
										with PDCFAGarbageCollector 
										with IPDSMachinery 
										with DyckStateGraphMachinery 
										with DSGAnalysisRunner 
										with FancyOutput 
										with DalvikAnalysisStatistics 
										with LiveRegisterAnalysis {

  type Term = Stmt
  


  //don't need this 
  //type Value = Value

  override type Kont = List[Frame]

  // OK.stil not sure about the switch frames
  // but the javascript PDCFA runner will set to be true
  def canHaveSwitchFrames = false

  // neither is the store senseitive
  def isStoreSensitive(s: ControlState) = true

  def step(q: ControlState, k: Kont, frames: Kont, store: SharedStore) = {
    val result = stepIPDS(q, k, frames)
    result.map {
      case (x, y) => (x, y, store)
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

  /**
   * Run Pushdown Control Flow Analysis
   * @param opts analysis options
   * @param anast inital expresion in ANF
   */
  def runPDCFA(opts: AIOptions, entryStmt: Stmt, methP: String) {
  
    val firstTime = (new java.util.Date()).getTime

    val (resultDSG, _) = evaluateDSG(entryStmt, methP)

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

    if (opts.verbose && opts.dumpGraph) {
      val path = getGraphParentFolder(opts)
      val res = prettyPrintDSG(resultDSG, path)
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
   // val (allVars, singletons) = computeSingletons(resultDSG.nodes, anast)
    val interrupted = opts.interrupt && resultDSG.nodes.size > opts.interruptAfter

  /*  dumpStatistics(opts, CFAStatistics(delta, //sizeExp, //allVars.size,
   //   singletons.size, 
      resultDSG.nodes.size, resultDSG.edges.size, interrupted))*/
    
    val (varPointsto, throwPointsto) = computePointsToStatistics(resultDSG.nodes) // is this states?
 
    val analysisStatistics = AnalysisStatistics(delta,  varPointsto, throwPointsto,  resultDSG.nodes.size, resultDSG.edges.size, interrupted)
      dumpStatisticsNew(opts, analysisStatistics)
    

    if (opts.dumpGraph) {
      println()
      println("Writing Dyck State Graph")
      println()
      val path = dumpDSGGraph(opts, resultDSG)
      println("Dyck State Graph dumped into " + path)

    }
  }
}



