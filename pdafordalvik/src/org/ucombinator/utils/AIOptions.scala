package org.ucombinator.utils
import org.ucombinator.utils.AnalysisType
import org.ucombinator.dalvik.testdriver.TestScripts
import java.io.File
import org.ucombinator.playhelpers.IRExtractHelper
import sys.process._
import scala.util.matching.Regex


class AIOptions {

  var sexprDir: String = null;
  var apkProjDir: String = null;
  var help: Boolean = false;
  var k: Int = 1
  var m: Int = 1
  var printStates = true
  var flatPolicy = "m"
  var analysis = "flat"
  var analysisType = AnalysisType.PDCFA
  var verbose = true
  var dumpStatistics = true
  var simplifyGraph = false
  var dummy = false
  var gc = true
  var gcDebug = false
  var dumpGraph = true
  var lang = "dalvik"
  var interrupt = false
  var interruptAfter = 250

  var timeInterrupt: Boolean = false
  var interruptAfterTime: Long = 900000
  var doLRA = true 
  var doRegex = false
  var regex: Regex = null

  var doCheckList = false
  var checkList: Set[String] = Set[String]()

  var graphDirName: String = apkProjDir + File.separator + "graphs"
  var statsDirName: String = apkProjDir + File.separator + "statistics"
  var permReportsDirName: String = apkProjDir + File.separator + "reports"
  var svgFilePath = ""
  var dotFilePath = ""
  var statsPath = ""
  var permReportPath = ""
  var heatMapReportPath = ""
  var securityReportPath = ""  
    
}

object AIOptions {

 // var noOfEdges = 0
  //var noOfStates = 0
  var debugInfo: Boolean = false
  
   def parseInApk(filePath: String) : (String, String)  = {
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
    // val et: ExtractIRHelperThread = new ExtractIRHelperThread(getIRCmdStr)
    //  IRExtractHelper.parseThread = et
     // et.start() 
      
      getIRCmdStr !
      
      var projFolder = fileFoldnerName 
      var  irfolder= fileFoldnerName + File.separator + "dedexout"
      (irfolder, projFolder) 
     
    }
  
   def parse(args: List[String], opts: AIOptions) {
    args match {
      case List() => {}
      case "--k" :: k :: rest => {
        opts.k = Integer.parseInt(k)
        parse(rest, opts)
      }

      case "--lang" :: lan :: rest => {
        lan match {
          case "js" => opts.lang = "js"
          case _ => opts.lang = "dalvik"
        }
        parse(rest, opts)
      }

      case "--help" :: rest => {
        opts.help = true
        parse(rest, opts)
      }

      case "--dummy" :: rest => {
        opts.dummy = true
        parse(rest, opts)
      }

      case "--simple-graph" :: rest => {
        opts.simplifyGraph = true
        parse(rest, opts)
      }

      case "--dump-statistics" :: rest => {
        opts.dumpStatistics = true
        parse(rest, opts)
      }

      case "--kcfa" :: rest => {
        opts.analysisType = AnalysisType.KCFA
        parse(rest, opts)
      }

      case "--pdcfa" :: rest => {
        opts.analysisType = AnalysisType.PDCFA
        parse(rest, opts)
      }

      case "--gc" :: rest => {
        opts.gc = true
        parse(rest, opts)
      }
      
       case "--lra" :: rest => {
        opts.doLRA = true
        parse(rest, opts)
      }

      case "--gcDebug" :: rest => {
        opts.gcDebug = true
        parse(rest, opts)
      }

      case "--verbose" :: rest => {
        opts.verbose = true
        parse(rest, opts)
      }

      case "--m" :: m :: rest => {
        opts.m = Integer.parseInt(m)
        parse(rest, opts)
      }

      case "--flat-policy" :: s :: rest => {
        opts.flatPolicy = s
        parse(rest, opts)
      }

      case "--analysis" :: a :: rest => {
        opts.analysis = a
        parse(rest, opts)
      }

      case "--print-states" :: "true" :: rest => {
        opts.printStates = true
        parse(rest, opts)
      }

      case "--print-states" :: "false" :: rest => {
        opts.printStates = false
        parse(rest, opts)
      }

      case "--dump-graph" :: rest => {
        opts.dumpGraph = true
        parse(rest, opts)
      }

      case "--interrupt-after" :: v :: rest => {
        opts.interrupt = true
        opts.interruptAfter = Integer.parseInt(v)
        parse(rest, opts)
      }
      
       case "--interrupt-after-time" :: v :: rest => {
        opts.timeInterrupt = true
        opts.interruptAfterTime = Integer.parseInt(v) * 60 *1000
        parse(rest, opts)
      }
      
     
      
      case "--regex" :: str :: rest =>{
        opts.doRegex= true
        opts.regex = str.r
        parse(rest, opts)
      }
      
      case "--checklist" :: list ::rest =>{
        opts.doCheckList= true
        opts.checkList =  CommonUtils.parseProperties(list)
         parse(rest, opts)
      }

      case fileName :: rest => {
        
         val (irFolder, profolder) = parseInApk(fileName) 
         opts.sexprDir = irFolder  
      
         opts.apkProjDir = profolder 
        
     //   opts.sexprDir = fileName;
        parse(rest, opts)
      }

    }
  }

  def parse(args: Array[String]): AIOptions = {
    val opts = new AIOptions
    parse(args.toList, opts)
    opts
  }
 
}

