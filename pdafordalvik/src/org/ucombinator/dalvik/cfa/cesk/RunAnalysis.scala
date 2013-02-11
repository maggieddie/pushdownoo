package org.ucombinator.dalvik.cfa.cesk


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

/**
 * @author shuying
 */
import org.ucombinator.utils.AIOptions
import org.ucombinator.dalvik.cfa.pdcfa.PDCFAAnalysisRunner
import org.ucombinator.utils.AnalysisType
import org.ucombinator.dalvik.vmrelated.APISpecs
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.dalvik.informationflow.DalInformationFlow
import org.ucombinator.dalvik.testdriver.TestScripts
import org.ucombinator.utils.CommonUtils
import org.ucombinator.playhelpers.PlayHelper
import org.ucombinator.playhelpers.AnalysisHelperThread


object RunAnalysis {

  val version: String = "20130124" 
  val versionString: String = "    Version " + version + "\n"

  val helpMessage = (" PushdownooExflow - a runner for Pushdown k-CFA with optional Abstract Garbage Collection \n" +
    versionString +
    """
    Usage (for a prebuilt jar with Scala SDK included):

    java -jar  PushdownOO_Exflow.jar [--lang lang][--pdcfa | --kcfa] [--k k] [--gc] [--lra] [--verbose] [--dump-graph] [--dump-statistics] [--simple-graph] [--interrupt-after n] [--help] filePath

    where   

    --lang l               			Target language (default = dalvik)
    --pdcfa                			run Pushdown k-CFA (run by default)
    --kcfa                	 		run classic k-CFA
    --k k                 			"k-degree" of the analysis, by default k = 0, only k = 0,1 are supported so far
    --gc                  	 		switch on abstract Garbage Collector (default = off)
    --lra  				   			switch on live register analysis
    --ppw n				   			per-point widening
    --aco 				   			aggresive cut off based on approximation
    --godel							abstract domain backed by Godel hashing (can be significantly fast)
    --non-null-check				mainly used to verify non-nullability for Android core libraries.
    --nn-unlinked					again, for verifying non-nullability of Android core libraries, don't use it if you're analyzing Android application
    --dump-graph           			dump Transition/Dyck State Graphs into a GraphViz file (default is off) 
    --dump-statisitcs      			dump analysis statistics into ./statistics/filename/stat-(analysis-type).txt
    --simple-graph         			if the graph is dumped, distinct natural numbers are displayed on its nodes instead of actual configurations
    --interrupt-after n    			interrupts the analysis after n states computed (default = off)
    --interrupt-after-time n(min) 	interrupts the analysis after n minutes (default = off)
    --for-intent-fuzzer				specialized static analysis for intent fuzzer
    --intraprocedural				intra-procedrual analysis, fast, imprecise, may suffice in some cases, such as fuzzer
    ----dump-paths 					prints out all the possible reachable paths. Each paths will have limited state information presented. It is for intent fuzzer to exrtact flows that involes intent operations. It is also preferrable when the graph representation is too big
    --help                 			print this message
    --verbose              			print additional information on the analysis and results
    --exceptions					turn on exception flow analysis as well (default = off)
    filePath               			path to an IR folder to be analysed
    """)

  def main(args: Array[String]) { 
    
    
    
      val at = new AnalysisHelperThread(args.drop(1))//fs,loc,pic,personal,ntw)
            at.start()
    
  /* //val opts = AIOptions.parse(args)
   
    val opts = setOptsForTest() 

   // if (args.size == 0 || opts.help) {
   //   println(helpMessage)  
  // return
  //}

  
    if (opts.sexprDir == null) {
      println()
      System.err.println("Please, specify a the app directory to process")
      println()
      println(helpMessage)
    }

    if (opts.lang == "js") {
      // processJS(opts)
    } else {
      processDalvik(opts)
    }
    //}
    
    // for each of the resinits, we run the runLRAOnListSts
     def doPreAnalysis(initEns: List[Stmt], resInits: List[Stmt], runner: PDCFAAnalysisRunner) {
    	println("start lra for inits...") 
      
    	runner.runLRAOnListSts(resInits) 
    	
    	println("lra on rest  inits...")
    	
    	initEns.foreach(runner.runLRAEntryBodies)
     
    	runner.runLRAOnAllMethods

    	println("Done with LRA preanalysis!")
    }

    def processDalvik(opts: AIOptions) {

      import org.ucombinator.dalvik.syntax._

      val sd = opts.sexprDir
      if (opts.verbose) {
        System.err.print("Parsing s-expressions...")
      }

      opts.analysisType match {
        case AnalysisType.PDCFA => {
          val runner = new PDCFAAnalysisRunner(opts)
          
          val (initEns, allIndividualInits) = runner.getListofInitEntries(opts)
          
    
          
          if(opts.doLRA){
            // do lra
             doPreAnalysis(initEns, allIndividualInits, runner)
            
             // parse in security related files
             DalInformationFlow.parseInAndroidKnowledge  
             
             // read JVm report
             APISpecs.readInReport 
             
             // starts to run Analysis
             runner.runPDCFA(opts,  initEns)
          
          } else  // no lra, 
          {
              APISpecs.readInReport
              runner.runPDCFA(opts, initEns)
              
          }
        }
      }
      System.exit(0)
    }*/
  }
  private def setOptsForTest(): AIOptions = {
    val opts = new AIOptions()

    val (irFolder, profolder) = TestScripts.parseInApk("./testapks/sanity.apk")

    opts.sexprDir = irFolder //"./benchmarks/dedexed_tests/sexps_ucm"
    //Goal
    //opts.sexprDir = "./tests/sexps_mf"   
    opts.apkProjDir = profolder

    opts
  }

}