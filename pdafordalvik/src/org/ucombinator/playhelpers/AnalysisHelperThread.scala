package org.ucombinator.playhelpers
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.utils.AIOptions
import org.ucombinator.dalvik.cfa.pdcfa.PDCFAAnalysisRunner
import org.ucombinator.utils.CommonUtils
import org.ucombinator.dalvik.syntax.DalvikClassDef
import org.ucombinator.dalvik.syntax.LabelStmt
import scala.collection.immutable.{ Set => ImmSet, Map => ImmMap }
import org.ucombinator.dalvik.syntax.StForEqual
import models.PermissionPair
import scala.tools.nsc.io.File
import org.ucombinator.utils.CommonUtils.HeatPair
import models.PropertyCheckList
import org.ucombinator.dalvik.cfa.cesk.StmtForEqual
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.cfa.widening.DalvikWideningConfiguration
import org.ucombinator.dalvik.cfa.widening.WideningHelperTrait
import scala.util.matching.Regex
import org.ucombinator.dalvik.syntax.FieldAssignStmt

// changed to support cmd line
class AnalysisHelperThread(params: Array[String]) extends Thread with WideningHelperTrait {

  import org.ucombinator.domains.CommonAbstractDomains._
  
  
  var exculsiveLibPatterns : List[Regex]= List[Regex]() 
  
  // for information flow
  var sources: Set[(String, String)] = Set[(String, String)]()
  var sinks: Set[(String, String)] = Set[(String, String)]()
  var sensitiveStrings: Set[(Regex, String)] = Set[(Regex, String)]()

  var permissionMap: scala.collection.mutable.Map[String, PermissionPair] = scala.collection.mutable.Map[String, PermissionPair]()

  var heatMap: scala.collection.mutable.Map[StForEqual, HeatPair] = scala.collection.mutable.Map[StForEqual, HeatPair]()

  var riskRankingMap: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map[String, Int]()
  // the meta information
  var classTable: Map[String, DalvikClassDef] = Map.empty
  var ppwWideningCounterTbl: Map[StForEqual, Int] = Map()
  //  var ppwWideningStoreTbl:  Map[StForEqual, (StateSpace#Store, StateSpace#PropertyStore)] = Map[StForEqual, (StateSpace#Store, StateSpace#PropertyStore)]()
  var ppwWideningStoreTbl: Map[StForEqual, (Store, PropertyStore)] = Map[StForEqual, (Store, PropertyStore)]()
  //
  var stmtMap: Map[String, LabelStmt] = Map.empty
  // def register(label: String, lst: LabelStmt) {
  //  Stmt.stmtMap += (label -> lst)}

  var liveMap: ImmMap[StForEqual, Set[String]] = ImmMap()
  
  var nullRefMap: Map[Stmt, Int] = Map.empty
  
  // for each class method, if they have intent processing fields mapping.
  var receivingIntentProcessingMap : Map[(String, String, Stmt), Map[String, Set[IntentExtraKeyTypeAndValue]]]= Map.empty
  
  var reachablePaths : Set[List[Int]]= Set[List[Int]]()
  // statistics
  var noOfEdges = 0
  var noOfStates = 0

  var curThreadStartTime: Long = 0

  var gopts: AIOptions = null

  var declaredPerms: List[String] = List[String]()

  object Primes {

    private val confidence = 20

    /*
   * It jsut generates an infinite number of prime numbers, and
   * and we can take whatever when we want to use it.
   */
    val primes: Stream[BigInt] = primesAfter(2)

    def primesAfter(n: BigInt): Stream[BigInt] = {
      var p = n

      if (n == 2) {
        return Stream.cons(2, primesAfter(3))
      }

      while (!p.isProbablePrime(confidence)) {
        p += 2
      }

      Stream.cons(p, primesAfter(p + 2))
    }
  }

  override def run() {

    PlayHelper.doAnalysis(params)

    //runner.runPDCFA(opts,  initEns) 
  }

}
/*class AnalysisHelperThread(k: Option[Int],
    gc: Option[String],
    doStateCutOff: Option[String],
    stateCutoff: Int,
     doTimeCutoff: Option[String],
    timeCutoff: Int,
  //  verbose: Option[String],
    path: String,
    doRegexO: Option[String],
    regexStr:String,
    doCheckList: Option[String],
    pl: PropertyCheckList
    fs:String,
     loc:String,
      pic:String,
       dev:String,
        ntw:String
    )

//(opts: AIOptions, initEns: List[Stmt], runner: PDCFAAnalysisRunner) 
    extends Thread{

  *//**
   * Bandage to move to multi-thread: thread local
   *//*
  // for information flow
  var sources: Set[(String, String)] = Set[(String, String)]()
  var sinks: Set[(String, String)] = Set[(String, String)]()
  var sensitiveStrings: Set[(String, String)] = Set[(String, String)]()
  
  var permissionMap: scala.collection.mutable.Map[String, PermissionPair] = scala.collection.mutable.Map[String, PermissionPair]()

  var heatMap: scala.collection.mutable.Map[StForEqual, HeatPair] = scala.collection.mutable.Map[StForEqual, HeatPair]()
  // the meta information
  var classTable: Map[String, DalvikClassDef] = Map.empty

  //
  var stmtMap: Map[String, LabelStmt] = Map.empty
  // def register(label: String, lst: LabelStmt) {
  //  Stmt.stmtMap += (label -> lst)}

  var liveMap: ImmMap[StForEqual, Set[String]] = ImmMap()
  var noOfEdges = 0
  var noOfStates = 0
  
  var curThreadStartTime:Long = 0
  
   var gopts: AIOptions = null
   
   var declaredPerms : List[String] = List[String]()
   
   

  
  
  override  def  run() {  
    val lstParams = PlayHelper.parseParameters(
        k, gc, doStateCutOff, stateCutoff, doTimeCutoff, timeCutoff, path,doRegexO ,regexStr, doCheckList, 
        pl
        fs,
     loc,
      pic,
       dev,
        ntw)
        val analysisParams = PlayHelper.parseAnalystParameters(lstParams)
        PlayHelper.doAnalysis(analysisParams)
     //runner.runPDCFA(opts,  initEns) 
   }
}*/