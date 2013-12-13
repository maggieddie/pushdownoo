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
  var verbose = false
  var dumpStatistics = true
  var simplifyGraph = false
  var dummy = false
  var gc = true
  var gcDebug = false
  
  // per point widening
  var ppw: Boolean = false 
  var ppwFrq : Int = 2
  // aggresive cut off
  var aco: Boolean  = false
  
  
  var dumpGraph = false
  var lang = "dalvik"
  var interrupt = false
  var interruptAfter = 250

  var timeInterrupt: Boolean = false
  var interruptAfterTime: Long = 900000
  var doLRA = false 
  
  var godel = false
 
  // currently for Android core library, not for Android apps
  var doNotNullCheck = false
  var unlinkedNonNull = false
  var initTopNull = false
  
 // for intent fuzzer
  var forIntentFuzzer = false
  
  var intraprocedural = false
  
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
  var riskRankingReportPath = ""
    
    var clsRiskRankingReportPath = ""
   var methRiskRankingReportPath = ""
   var obranches = false
   var brNum = 0
   var brCutoff = 0
   
   var exception = false
   
   var printPaths = false
    
}

object AIOptions {

  var debugInfo: Boolean = false
  
   def parseInApk(filePath0: String, doNotNull: Boolean): (String, String)  = {
    println("filePath0", filePath0)
    val filePath = filePath0.replace(" ", "-")
    println("filePath", filePath)
      val lst = filePath.split("/").toList
      val plen = lst.length
   
      val fileParts = filePath.split("\\.apk").toList
      val fileFoldnerName = fileParts.head 
    
      println("Project Name to Analyzer::::::::::::: "+ fileFoldnerName)
      val parts = fileFoldnerName.split("/").toList
      val l = parts.length
      val fileName = 
      if( l>0)
    	  parts(l-1).replace(" ", "-")
      else  println("WARNING: not geting APK file NAme!")
      println("fileName", fileName)
      
      val pathToScript0  = lst.dropRight(1).foldLeft("")((res, s) => {res + s + "/"})
      val pathToScript = pathToScript0.replace(" ", "-")
      println("pathToScript", pathToScript)
     val getIRCmdStr = if(doNotNull) {
         "/usr/bin/python ./getIR.py" + " " + "--donull" + " " + pathToScript + " " + fileName
       } else{
         "/usr/bin/python ./getIR.py" + " " + "--nonull" +  " " +  pathToScript + " " + fileName
       }
      
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
      
      case "--godel" :: rest => {
        opts.godel = true
        parse(rest,opts)
      }

      case "--gc" :: rest => {
        opts.gc = true
        parse(rest, opts)
      }
      
       case "--lra" :: rest => {
        opts.doLRA = true
        parse(rest, opts)
      }
       
       case "--ppw" :: num:: rest => {
         opts.ppw = true
         opts.ppwFrq = Integer.parseInt(num)
         parse(rest, opts)
       } 
       
       case "--aco" ::  rest => {
         opts.aco = true 
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
      
      case "--non-null-check" :: rest => {
        opts.doNotNullCheck = true
        parse(rest, opts)
      }
      
      case "--nn-unlinked" :: rest => {
         opts.doNotNullCheck = true
         opts.unlinkedNonNull = true
        parse(rest, opts)
      }

      // this option should be after the above two
       case "--init-topNull" :: rest => {
        if(opts.doNotNullCheck){
         opts.initTopNull = true
        }
        parse(rest, opts)
      } 
       
       case "--for-intent-fuzzer" :: rest => {
    	   opts.forIntentFuzzer = true
    	   opts.printPaths = true
    	   parse(rest, opts)
      } 
       
       case  "--intraprocedural" :: rest =>{
         opts.intraprocedural = true
         opts.obranches = true
         parse(rest, opts)
       }
       
       case "--obranches" :: cut:: rest => {
    	  opts.obranches = true
    	  opts.brCutoff = Integer.parseInt(cut)
    	 
         parse(rest, opts)
       }
       
        case "--exceptions" ::  rest => {
    	  opts.exception = true
    	//  opts.intraprocedural = false
         parse(rest, opts)
       }
       
       case "--dump-paths" :: rest =>{
         opts.printPaths = true
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

      // make the file name as the last parameter!!!
      // and so that the notnull can check if it is true or not
     // because it will decide whether to let the disassembler to load the memory consuming odex dependencies!
      case fileName :: rest => {
         val fileName0 = fileName.replace(" ", "-") 
         val (irFolder, profolder) = 
            if(opts.doNotNullCheck) parseInApk(fileName0, true)
            else parseInApk(fileName0, false)
         //= parseInApk(fileName0) 
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

