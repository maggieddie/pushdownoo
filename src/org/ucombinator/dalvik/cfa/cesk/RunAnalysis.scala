package org.ucombinator.dalvik.cfa.cesk
import org.ucombinator.utils.AIOptions
import org.ucombinator.dalvik.cfa.pdcfa.PDCFAAnalysisRunner
import org.ucombinator.utils.AnalysisType
import org.ucombinator.dalvik.vmrelated.APISpecs


object RunAnalysis {

  val version: String = "20130124"//"20121125"
  val versionString: String = "    Version " + version + "\n"

  val helpMessage = (" GenericCFA - a runner for k-CFA and Push-down k-CFA with optional Abstract Garbage Collection \n" +
    versionString +
    """
    Usage (for a prebuilt jar with Scala SDK included):

    java -jar GenericCFA.jar [--lang lang][--pdcfa | --kcfa] [--k k] [--gc] [--verbose] [--dump-graph] [--dump-statistics] [--simple-graph] [--interrupt-after n] [--help] filePath

    where

    --lang l               Target language (default = dalvik)
                             js        -- LambdaJS
                             scheme    -- Dalvik
    --pdcfa                run Pushdown k-CFA (run by default)
    --kcfa                 run classic k-CFA
    --k k                  "k-degree" of the analysis, by default k = 0, only k = 0,1 are supported so far
    --gc                   switch on abstract Garbage Collector (default = off)
    --dump-graph           dump Transition/Dyck State Graphs into a GraphViz file ./graphs/filename/graph-(analysis-type).gv
    --dump-statisitcs      dump analysis statistics into ./statistics/filename/stat-(analysis-type).txt
    --simple-graph         if the graph is dumped, distinct natural numbers are displayed on its nodes instead of actual configurations
    --interrupt-after n    interrupts the analysis after n states computed (default = off)
    --help                 print this message
    --verbose              print additional information on the analysis and results
    filePath               path to a file to be analysed
    """)

  def main(args: Array[String]) {

    //val opts = AIOptions.parse(args)
    
    val opts = setOptsForTest()

 /*   if (args.size == 0 || opts.help) {
      println(helpMessage)
      return
    }*/

  
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

    def processDalvik(opts: AIOptions) {

      import org.ucombinator.dalvik.syntax._

      val sd = opts.sexprDir
      if (opts.verbose) {
        System.err.print("Parsing s-expressions...")
      }

      opts.analysisType match {
        case AnalysisType.PDCFA => {
          val runner = new PDCFAAnalysisRunner(opts)
          
          val (methpath, entry, allIndividualInits) = runner.getAndroidEntry4Test(opts)
          
          if(opts.doLRA){
             println("start lra inits")
            runner.runLRAOnListSts(allIndividualInits)
            println("finished")
             /**
           * we'll need to run the lra on the entries bodies
           */
            val  (methPath2, entry2 )  = runner.runLRAEntryBodies(entry)
            
             runner.runLRAOnAllMethods
             
                println("Done with LRA preanalysis!")
         // Stmt.liveMap.foreach(println)
          // for exceotions
          APISpecs.readInReport
          
          //APISpecs.apiSpecTable.foreach(println)
          
          runner.runPDCFA(opts, entry, methPath2)
          
          } else {
              APISpecs.readInReport
              runner.runPDCFA(opts, entry, methpath)
          }
        
        }
        // case KCFA => {
        //    val runner = new KCFAAnalysisRunner(opts)
        //   runner.runKCFA(opts, anast)
        // }
      }

    }
  }
  private def setOptsForTest() : AIOptions = {
    val opts = new AIOptions()
   opts.sexprDir = "./benchmarks/dedexed_tests/sexps_pmdseq2"  
   //Goal
    //opts.sexprDir = "./tests/sexps_mf"   
      opts
  }

}