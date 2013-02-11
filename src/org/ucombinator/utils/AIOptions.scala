package org.ucombinator.utils
import org.ucombinator.utils.AnalysisType

class AIOptions {
   
    var sexprDir: String = null;
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
  var dumpGraph = false
  var lang = "dalvik"
  var interrupt = true
  var interruptAfter = 30000
  var doLRA = false
  
  
}

object AIOptions {

  var debugInfo: Boolean = false
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

      case fileName :: rest => {
        opts.sexprDir = fileName;
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

