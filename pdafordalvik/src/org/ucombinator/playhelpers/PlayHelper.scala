package org.ucombinator.playhelpers
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.dalvik.cfa.pdcfa.PDCFAAnalysisRunner
import org.ucombinator.utils.AIOptions
import org.ucombinator.utils.AnalysisType
import org.ucombinator.dalvik.informationflow.DalInformationFlow
import org.ucombinator.dalvik.vmrelated.APISpecs
import org.ucombinator.dalvik.testdriver.TestScripts
//import play.api.libs.json._
import java.io.File
import org.ucombinator.utils.CommonUtils
import models.PropertyCheckList

object PlayHelper {

 // var gopts: AIOptions = null
  
  
  val version: String = "20130124"
  val versionString: String = "    Version " + version + "\n"
  val helpMessage = (" PushdownooExflow - a runner for Pushdown k-CFA with optional Abstract Garbage Collection \n" +
    versionString +
    """
    Usage (for a prebuilt jar with Scala SDK included):

    java -jar  PushdownOO_Exflow.jar [--lang lang][--pdcfa | --kcfa] [--k k] [--gc] [--lra] [--verbose] [--dump-graph] [--dump-statistics] [--simple-graph] [--interrupt-after n] [--help] filePath

    where   

    --lang l               Target language (default = dalvik)
    --pdcfa                run Pushdown k-CFA (run by default)
    --kcfa                 run classic k-CFA
    --k k                  "k-degree" of the analysis, by default k = 0, only k = 0,1 are supported so far
    --gc                   switch on abstract Garbage Collector (default = off)
    --lra  				   switch on live register analysis
    --dump-graph           dump Transition/Dyck State Graphs into a GraphViz file ./graphs/filename/graph-(analysis-type).gv
    --dump-statisitcs      dump analysis statistics into ./statistics/filename/stat-(analysis-type).txt
    --simple-graph         if the graph is dumped, distinct natural numbers are displayed on its nodes instead of actual configurations
    --interrupt-after n    interrupts the analysis after n states computed (default = off)
    --help                 print this message
    --verbose              print additional information on the analysis and results
    filePath               path to an IR folder to be analysed
    """)
    
    /*// result links
     def constrJsonResult : JsValue = {
    
     def addAssetsToPath (str: String) : String = {
       val noPublic = str.split("/").toList.drop(2).foldLeft("")((res, s) => {res + s + "/"})
       val noPublicSub = noPublic.substring(0, noPublic.length-1)
      File.separator + "assets" + File.separator + noPublicSub
    }
     if(Thread.currentThread().asInstanceOf[AnalysisHelperThread].gopts != null) {
    	 val statsP = addAssetsToPath(Thread.currentThread().asInstanceOf[AnalysisHelperThread].gopts.statsPath)
    	 val svgP = addAssetsToPath(Thread.currentThread().asInstanceOf[AnalysisHelperThread].gopts.svgFilePath)
     
    	 println("statics Paths", statsP)
    	 println("graph Paths", svgP)
     
    	 Json.toJson(
    			 Seq(Json.toJson(
    					 Map("link"->Json.toJson(statsP),  "type" -> Json.toJson("Analysis Statistics")) 
    			 	),
    			 	Json.toJson(
    			 			Map("link"->Json.toJson(svgP),  "type" -> Json.toJson("Analysis State Graph")) 
    			 	)//,
    			 	//Json.toJson(
    			 		//	Map("link"->Json.toJson("/assets/apks/1.svg"),  "type" -> Json.toJson("SVG Not Ready")) 
    			 	//)
    			 )
    		 )
     }else {
       
    	 JsNull
     }
  }*/
  
  private def setPaths(opts:AIOptions) : AIOptions =  {
     
    opts. statsDirName = opts.apkProjDir +  File.separator +  "statistics"
    opts.graphDirName =  opts.apkProjDir +  File.separator +  "graphs"
    opts.permReportsDirName = opts.apkProjDir + File.separator + "reports"
    
    val (dfp, sfp) = CommonUtils.getGraphFolderFileNames(opts)
      opts.dotFilePath = dfp
      opts.svgFilePath = sfp    
      opts.statsPath = CommonUtils.getStatisticsDumpFolderFileName(opts)
      opts.permReportPath  = CommonUtils.getReportDumpFolderFileName(opts)
      opts.heatMapReportPath = CommonUtils.getHeatDumpFolderFileName(opts)
      opts.securityReportPath = CommonUtils.getSecurityDumpFolderFileName(opts)
      opts.riskRankingReportPath = CommonUtils.getRiskRankingFolderFileName(opts)
      opts
    
  }
    

  /*
    * that create a background thread to start the analyzer. 
    * and at the same time, we will returns the possible files 
  	* links and correspondent message. 
  	* and this function needs to return a json value.
  	* 
  	* 1. k;
  	* 2. gc;
  	* 3. statecutoff;
  	* 4. verbose
    */

  def doAnalysis(args: Array[String])   {

    val opts0 = AIOptions.parse(args)
     
    //IRExtractHelper.parseThread.join()
    
    val opts = setPaths(opts0)

    Thread.currentThread().asInstanceOf[AnalysisHelperThread].gopts = opts
    //val opts = setOptsForTest() 

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

    processDalvik(opts)

    // for each of the resinits, we run the runLRAOnListSts
    def doPreAnalysis(initEns: List[Stmt], resInits: List[Stmt], runner: PDCFAAnalysisRunner) {
      println("start lra for inits...")

      runner.runLRAOnListSts(resInits)

      println("lra on rest  inits...")

      initEns.foreach(runner.runLRAEntryBodies)

      runner.runLRAOnAllMethods

      println("Done with LRA preanalysis!")
    //   Thread.currentThread().asInstanceOf[AnalysisHelperThread].liveMap.foreach(println)
      
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

          if (opts.doLRA) {
            // do lra
            doPreAnalysis(initEns, allIndividualInits, runner)

            // parse in security related files
            DalInformationFlow.parseInAndroidKnowledge

            // read JVm report
            APISpecs.readInReport

            // starts to run Analysis
            runner.runPDCFA(opts, initEns)
            

          } else // no lra, 
          {
            APISpecs.readInReport
            runner.runPDCFA(opts, initEns)
          }
        }
      }
      //System.exit(0)
    }
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

  def parseAnalystParameters(clientParams: List[String]): Array[String] = {
    val kStr = clientParams(0)
    val gcStr = clientParams(1)
    val doStateCutoff = clientParams(2)
    val interruptStNo = clientParams(3)
    
    val doTimeCutoff = clientParams(4)
    val timeCutoff = clientParams(5)
    
   // val debugStr = clientParams(4)
    val apkPath = clientParams(6)
    
    val doRegex = clientParams(7)
    val regex = clientParams(8)
    
    val doCheckList = clientParams(9)
    //val pl = clientParams(10)
    
    val fs = clientParams(10)
    val loc = clientParams(11)
    val pic = clientParams(12)
    val device = clientParams(13)
    val network = clientParams(14)
    val display = clientParams(15)
    val thread = clientParams(16)
    val ipc = clientParams(17)
    val contact = clientParams(18)
    val sensor = clientParams(19)
    val account = clientParams(20)
    val media = clientParams(21)
    val sms = clientParams(22)

    var constrParams = List("--k", kStr)

    if (gcStr == "true") {
      constrParams = constrParams ::: List("--gc", "--lra")
    }
    
    //default///
    
    constrParams = constrParams ::: List("--ppw", "2", "--aco")

    if (doStateCutoff == "true") {
      val ino = interruptStNo.toInt
      if (ino > 0) {
        constrParams = constrParams ::: List("--interrupt-after", ino.toString)
      }
    }
    
    if (doTimeCutoff == "true") {
      val ino = timeCutoff.toInt
      if (ino > 0) {
        constrParams = constrParams ::: List("--interrupt-after-time", ino.toString)
      }
    }
 
   
    if(doRegex == "true"){
      constrParams = constrParams ::: List("--regex", regex) 
    }
    
    var plString = "" 
    
    if(doCheckList == "false") 
    {
        constrParams = constrParams ::: List("")
    }
    
    else {
      
      constrParams = constrParams ::: List("--checklist")
      
      if(fs == "true") {
    	  plString = plString + "|" + "filesystem" + "|" +  "sdcard" + "|" +  "reflection"
          //constrParams = constrParams ::: List("filesystem", "sdcard", "reflection")
      }
      if(loc == "true") {
     	plString = plString + "|"+ "location" + "|" +  "reflection"
          //constrParams = constrParams ::: List("location", "reflection")
      }
      if(pic == "true") {
        plString = plString + "|"+ "picture" + "|" +  "reflection"
        //constrParams = constrParams ::: List("picture", "reflection")
      }
      if(device == "true") {
         plString = plString + "|"+ "deviceid" +  "|"+ "serialid" + "|" +  "reflection"
      }
      
      if(network == "true") {
          plString = plString + "|" + "network" + "|" +  "reflection"
      }
      
      if(display == "true") {
        plString = plString + "|" + "display" + "|" +  "reflection"
      }
      
      if(thread == "true"){
        plString = plString + "|" + "thread" + "|" +  "reflection"
      }
      
      if(ipc == "true") {
        plString = plString + "|" + "ipc"+ "|" +  "reflection"
      }
      
      if(contact == "true") {
        plString = plString + "|" + "contact" + "|" +  "reflection"
      }
      
      if(sensor == "true") {
        plString= plString + "|" + "sensor"+ "|" +  "reflection"
      }
      
      if(account == "true") {
        plString = plString + "|" + "account" + "|" +"browserbookmark" + "|" + "browserhistory" + "|" +  "reflection"
      }
      
      if(media == "true"){
        plString = plString + "|" + "media" + "|" + "voice"
      }
      
      if(sms == "true"){
        plString = plString + "|" + "sms" + "|" +  "reflection"
      }
       constrParams = constrParams ::: List(plString)
    }  
    
    constrParams = constrParams ::: List(apkPath)

    val res = constrParams.toArray
    println("the result is !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!>>>>>>>>>>>>>>>>>>>>>>>>: ")
    constrParams.foreach(println)
    res
    
  }

  def parseParameters(kStrO: Option[Int],
    gcStrO: Option[String],
    doStateCutoffO: Option[String],
    stateCutoff: Int,
      doTimeCutoffO: Option[String],
    timeCutoff: Int,
   // verboseO: Option[String],
    path: String,
    doRegexO: Option[String],
    regexStr: String,
    doCheckListO:Option[String],
    pl: PropertyCheckList
   /* fs:String,
    loc:String,
    pic:String,
    dev:String,
    net:String*/): List[String] = {

    val kStr = kStrO match {
      case Some(k) => k.toString
      case None => ""
    }
    val gcStr = gcStrO match {
      case Some(str) => { str }
      case None => ""
    }

    val doStateCutoff = doStateCutoffO match {
      case Some(str) => { str }
      case None => ""
    }
    
     val doTimeCutoff = doTimeCutoffO match {
      case Some(str) => { str }
      case None => ""
    }

  /*  val verboseStr = verboseO match {
      case Some(vStr) => {
        vStr
      }
      case None => {
        ""
      }
    }*/
    
    val doRegexStr = doRegexO match{
      case Some(vStr) => {
        vStr
      }
      case None => {
        ""
      }
    }
    
     val doCl = doCheckListO match{
      case Some(vStr) => {
        vStr
      }
      case None => {
        ""
      }
    }
    List(kStr, gcStr, doStateCutoff, stateCutoff.toString, doTimeCutoff, timeCutoff.toString,//verboseStr, 
        path, doRegexStr, regexStr, doCl, 
        pl.filesystem.toString, pl.location.toString, pl.picture.toString, pl.ids.toString, pl.network.toString,
        pl.display.toString, pl.thread.toString, pl.ipc.toString,pl.contact.toString,
        pl.sensor.toString,pl.account.toString,pl.media.toString,pl.sms.toString()
        /*fs:String,
    loc:String,
    pic:String,
    dev:String,
    net:String*/ )

  }

}