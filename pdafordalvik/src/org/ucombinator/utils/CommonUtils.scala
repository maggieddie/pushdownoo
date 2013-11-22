package org.ucombinator.utils
import org.ucombinator.dalvik.syntax._
import scala.util.Random
import scala.tools.nsc.io.Directory
import java.io.File
import org.ucombinator.playhelpers.PlayHelper
//import play.api.libs.json._
import org.ucombinator.playhelpers.AnalysisHelperThread
import models.PropertyCheckList



object CommonUtils {

  def main(args: Array[String]): Unit = {
    def genStmts(res: List[Stmt])(n : Int): List[Stmt] ={
      if (n == 0) res
      else
    	  	genStmts(new NopStmt(StmtNil, StmtNil, "", "") :: res)(n-1)
    }
    //var testStmts = genStmts(List())(5).reverse
    val s1 = new GotoStmt("100", StmtNil, StmtNil, "", "")
    val s2 = new GotoStmt("akjsdfsd", StmtNil, StmtNil, "", "")
    
    
    Debug.prntDebugInfo("linked ", linkingList(List()) (List(s1,s2)))
  }
  
    def  TestSNRet(so: Option[SName]) : SName = {
    so match {
      case Some(sn) => sn
      case None => {
        Debug.prntErrInfo("Matching SName returns None", "SName", None)
        SName.from("failed")
      }
    }
  }
    def extractStmts(oplt: List[Option[Stmt]]) : List[Stmt] ={
      val st1 = 
        oplt map ((ost : Option[Stmt]) => {
        ost match{
          case Some(s) => s
          case None => StmtNil
        } 
      })
      st1 filter ((s: Stmt) => {
        s match {
          case StmtNil => false
          case _ => true
        }
      })
    }
    
    def extractHead(os: Option[Stmt]) : Stmt = {
      os match{
        case Some(s) => s
        case None => StmtNil
      }
    }
    
    def flattenLinkedStmt (res: List[Stmt]) (stmt: Stmt) : List[Stmt] ={
      stmt match {
        case StmtNil => res ::: List(stmt)
        case _ => {
          val nxt = stmt.next
          flattenLinkedStmt(res ::: List(stmt))(nxt)
        }
      }
    }
     
      
  def linkedListWrapper(res: List[Stmt])(as: List[Stmt]) : Option[Stmt] = {
    if (as.length == 0) None
    else 
    if(as.length == 1)  Some(as.head)
    else Some(linkingList(List())(as))
  }
  
    
 def linkingList(res: List[Stmt])(as: List[Stmt]) : Stmt = {
    as match {
      case Nil => res.head
      case hd :: tl => { 
        tl match {
          case Nil => {
            res.head
          }
          case hdi :: tli => {
             hd.next = hdi
            linkingList(res ::: List(hd))(tl)
          }
        }
      }
    }
    }
 
 def getThisRegStr(mdRegs: BigInt, argsNum: BigInt) : String ={
      val thisRegIndex = mdRegs - argsNum - 1
     StringUtils.constrRegStr(thisRegIndex)
 }
    
   //ARRAY TO LIST   
  def toList[a](array: Array[a]): List[a] = {
    if (array == null || array.length == 0) Nil
    else if (array.length == 1) List(array(0))
    else array(0) :: toList(array.slice(1, array.length))
  }
  
   def isLineS(curS: Stmt) : Boolean ={
     curS match {
       case LineStmt(_,_,_,_,_) => true
       case _ => false
     }
  }
  
   
   def isLabelS(curS: Stmt) : Boolean = {
     curS match {
       case LabelStmt(_,_,_,_,_) => true
       case _ => false
     }
   }
   
   def isNop(s: Stmt) : Boolean = {
     s match {
       case NopStmt(_,_,_,_) => true
       case _ => false
     }
   }
  
   def findNextStmtNotLineOrLabel (curN: Stmt)  : Stmt={
     if(isLineS(curN) || isLabelS(curN) || isNop(curN)) {
       val toNext = curN.next
       findNextStmtNotLineOrLabel(toNext)
     }
     else
       curN
   }

    def randomizeLineNumberOneStmt(stmt: Stmt, clsPath: String, methP: String) : Stmt = {
    /*import util.Random.nextInt
    val seed = 500
    val res = Stream.continually(nextInt(seed)).toList
    val index = Stream.continually(nextInt(seed-1)).toList.take(1).head*/
   val rn = Random.nextInt()
    stmt.lineNumber = LineStmt(rn.toString, StmtNil, StmtNil, clsPath, methP)
    stmt
  }
    
    def getThrownLineNumer(clsP: String, methPath:String) : Stmt = {
      val magicNumForThrowLineNO = 100000
      val negRn = 0 - Random.nextInt(100000)
       LineStmt(negRn.toString, StmtNil, StmtNil, clsP, methPath)
    }
    
    def isAnnoThrownLineStmt(st: Stmt) : Boolean = {
     
      st  match {
        case ls@LineStmt(_, _, _,_,_) => {
          val numStr = ls.linenumber
          if(numStr.contains(".")) false
          else {
          val numNum = BigInt(numStr)
          if(numNum < 0 && numNum > -100000) true else false
          }
        }
        case _ => false
      }
    }
    
    def genNewThrownLineNumber(clsP: String, methP: String) : Stmt = {
       val magicNumForThrowLineNO = 1000
      val negRn = Random.nextFloat()
       LineStmt(negRn.toString, StmtNil, StmtNil, clsP, methP)
    }
    
  def isStringLibs(mp: String) : Boolean = {
    val setofLibs = Set("java/lang/StringBuilder/<init>", 
        "java/lang/StringBuilder/append",
        "java/lang/String/valueOf")
    if(setofLibs.contains(mp)) true else false
  }

  def isMetaLibCall(mp: String): Boolean = {
    val setofCalls = Set("java/lang/Class/getName", "java/lang/Class/forName")
    if (setofCalls.contains(mp)) true else false
  }
   
  def getAllRegsStrFromRegNum(regNum: BigInt) : List[String] = {
     List.range(BigInt("0"), regNum).map (StringUtils.constrRegStr)
  }
  
  def getRegStrsFromAExp(aexp: AExp) : Set[String]= {
   aexp match {
      case ae@RegisterExp(_) => { 
        val str = ae.regStr
        if(str.startsWith("v")){
        	Set(ae.regStr)}
        else Set()
      }
      case _ => {throw new Exception(" exception from getRegStrsFromAExp: not a RegisterExp, Found:" + aexp.toString)}
    }
  }
  
  def getRegStrsListFromAExpList(aexps: List[AExp]) : Set[String] = {
    val allRegExs =  aexps filter {
      case RegisterExp(_) => true
      case _ => false
    }
    allRegExs.foldLeft(Set[String]())((res, regE) => {
      res ++ getRegStrsFromAExp(regE)
    })
  }
  
  
   def constrDistinctStatementStr (st: Stmt) : String = {
     if(st == StmtNil) st.toString
     else
     if(st.next == StmtNil) 
       st.toString + "$"+ st.lineNumber 
      else st.toString + "$"+st.lineNumber + "$"+st.next
    
  }
   
    /**
   * Get a fancy name dump files
   * This will be modified to generate svg graph
   */
  def getGraphDumpFileName(opts: AIOptions): (String, String) = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "graph-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if(opts.doLRA) "-lra" else ""
    val dotFilePath = prefix + arity + cfa + gc + lrv + ".dot"
    val svgFilePath = prefix + arity + cfa + gc + lrv + ".svg"
    (dotFilePath, svgFilePath)
  }
  
  def getGraphFolderFileNames(opts: AIOptions) : (String, String) = {
    val (dfp, sfp) = getGraphDumpFileName(opts)
     val graphFolderPath =  opts.graphDirName
    val dotfilePath = graphFolderPath+ File.separator + dfp //CommonUtils.getGraphDumpFileName(opts)  
    val svgFilePath = graphFolderPath+ File.separator + sfp
    (dotfilePath, svgFilePath)
  }
  
  def getDumpFileName(opts: AIOptions, prefix: String): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    } 
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if(opts.doLRA) "-lra" else "" 
    val godel = if(opts.godel) "-godel" else ""
    prefix + arity + godel + cfa + gc + lrv + ".txt"
  }

  def getDumpFileName2(opts: AIOptions, prefix: String, ft: String): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if (opts.doLRA) "-lra" else ""
    val godel = if (opts.godel) "-godel" else ""

    if (ft == "html")
      prefix + arity + godel + cfa + gc + lrv + ".html"
    else
      prefix + arity + godel + cfa + gc + lrv + ".txt"
  }

// stupid methods duplicates, should be replaced with getDumpFileName
  def getStatisticsDumpFileName(opts: AIOptions): String = {
    println("called this statistics")
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "stat-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if(opts.doLRA) "-lra" else "" 
    val godel = if(opts.godel) "-godel" else ""
    prefix + arity + godel + cfa + gc + lrv + ".txt"
  }
  
  def getReportName(opts: AIOptions): String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "report-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if(opts.doLRA) "-lra" else "" 
        val godel = if(opts.godel) "-godel" else ""
    prefix + arity + godel + cfa + gc + lrv + "-least-permission" + ".txt"
  }
  
  def getHeatReportName(opts: AIOptions) :String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "report-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if(opts.doLRA) "-lra" else "" 
    prefix + arity + cfa + gc + lrv + "-heat-map" + ".html"
  }
  
   def getSecurityReportName(opts: AIOptions) :String = {
    val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "report-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if(opts.doLRA) "-lra" else "" 
        val godel = if(opts.godel) "-godel" else ""
    prefix + arity + godel + cfa + gc + lrv + "-security" + ".html"
  }
   
   def getRiskRankingReportName(opts: AIOptions) : String = {
     val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "report-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if(opts.doLRA) "-lra" else "" 
        val godel = if(opts.godel) "-godel" else ""
    prefix + arity + godel + cfa + gc + lrv + "-riskranking" + ".html"
   }
   
   def getClsRiskRankingReportName(opts: AIOptions) : String = {
     val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "report-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if(opts.doLRA) "-lra" else "" 
        val godel = if(opts.godel) "-godel" else ""
    prefix + arity + godel + cfa + gc + lrv + "-riskranking-classes" + ".html"
   }
   
   def getMethRiskRankingReportName(opts: AIOptions) : String = {
     val cfa = opts.analysisType match {
      case AnalysisType.KCFA => "-cfa"
      case AnalysisType.PDCFA => "-pdcfa"
    }
    val prefix = "report-"
    val arity = if (opts.dummy) "dummy" else opts.k.toString
    val gc = if (opts.gc) "-gc" else ""
    val lrv = if(opts.doLRA) "-lra" else "" 
        val godel = if(opts.godel) "-godel" else ""
    prefix + arity + godel + cfa + gc + lrv + "-riskranking-methods" + ".html"
   }
  
  
  
  def getStatisticsDumpFolderFileName(opts: AIOptions) : String ={
     
    opts.statsDirName + File.separator + getStatisticsDumpFileName(opts)
  }
  
  def getReportDumpFolderFileName(opts: AIOptions) : String = {
    opts.permReportsDirName + File.separator + getReportName(opts)
  }
  
  def getHeatDumpFolderFileName(opts: AIOptions) : String = {
     opts.permReportsDirName + File.separator + getHeatReportName(opts)
  }
  
  def getSecurityDumpFolderFileName(opts: AIOptions) : String = {
     opts.permReportsDirName + File.separator + getSecurityReportName(opts)
  }
  
  def getRiskRankingFolderFileName(opts : AIOptions) : String = {
    opts.permReportsDirName + File.separator + getRiskRankingReportName(opts)
  }
  
  def getClsRiskRankingFolderFileName(opts : AIOptions) : String = {
    opts.permReportsDirName + File.separator + getClsRiskRankingReportName(opts)
  }
  
  def getMethRiskRankingFolderFileName(opts : AIOptions) : String = {
    opts.permReportsDirName + File.separator + getMethRiskRankingReportName(opts)
  }
  
  // some utils to play
  def getStaticResultLinks(k: Option[Int],
    gcO: Option[String],
    doStateCutOff: Option[String],
    stateCutoff: Int,
    doTimeCutoff: Option[String],
    timeCutoff: Int,
   // verbose: Option[String],
    filePath: String, 
    doRegex:Option[String],
    regexStr: String,
    doCheckList:Option[String],
    pl: PropertyCheckList
     /*fs:String,
     loc:String,
      pic:String,
       dev:String,
        ntw:String*/) : List[String] = {
    
     val lstParams = PlayHelper.parseParameters(k, gcO, doStateCutOff, stateCutoff, doTimeCutoff, timeCutoff, filePath,doRegex, regexStr, doCheckList,
         pl)
         //fs,loc,pic,dev,ntw)
     
  
      val lst = filePath.split("/").toList
      val plen = lst.length
      
     val apkProjDir = filePath.split("\\.apk").toList.head 
   	var graphDirName: String = apkProjDir +  File.separator +  "graphs"
   	var statsDirName: String = apkProjDir +  File.separator +  "statistics"
   	var reportDirName : String= apkProjDir +  File.separator +  "reports"
       val prefix = "graph-"
    val arity =lstParams(0)
    val gc_lra = if (lstParams(1) == "true") "-gc-lra" else ""
      val svgfilePath = prefix + arity + "-pdcfa" + gc_lra + ".svg"
   
    val statfilePath = "stat-" + arity + "-pdcfa" + gc_lra + ".txt"
    
    val permReportFilePath = "report-" + arity + "-pdcfa" + gc_lra + "-least-permission" + ".txt"
    
    val heatMapReportFilePath = "report-" + arity + "-pdcfa" + gc_lra + "-heat-map" + ".html"
     
    val securityMapFilePath = "report-" + arity + "-pdcfa" + gc_lra + "-security" + ".html"
    
    val svgLink = graphDirName + File.separator + svgfilePath
    val statLink = statsDirName + File.separator + statfilePath
    val permReportPath = reportDirName +  File.separator + permReportFilePath
    val heatMapPath = reportDirName + File.separator + heatMapReportFilePath
    val secuPath = reportDirName + File.separator + securityMapFilePath
    
    println("*******" +statfilePath) 
     println("&&&&&&" + svgLink)
    List(statLink, permReportPath, secuPath , heatMapPath, svgLink)
    
  }
   def addAssetsToPath (str: String) : String = {
       val noPublic = str.split("/").toList.drop(2).foldLeft("")((res, s) => {res + s + "/"})
       val noPublicSub = noPublic.substring(0, noPublic.length-1)
      File.separator + "assets" + File.separator + noPublicSub
    }
   
  /* def constrJsonResult(k: Option[Int],
    gcO: Option[String],
    doStateCutOff: Option[String],
    stateCutoff: Int,
    doTimeCutoff: Option[String],
    timeCutoff: Int,
    //verbose: Option[String],
    filePath: String,
    doRegex: Option[String],
    regexStr: String,
    doCheckList:Option[String],
    pl: PropertyCheckList
     fs:String,
     loc:String,
      pic:String,
       dev:String,
        ntw:String) : JsValue = {
    
     println("filePath from ::::::::::::::::::", filePath)
     val resultList = getStaticResultLinks(k, gcO, doStateCutOff, stateCutoff, doTimeCutoff, timeCutoff, filePath, doRegex, regexStr,doCheckList, 
         pl)
         //fs,loc,pic,dev,ntw)
    
     println("%%%%%%%%%%%%%%%%%% result list",resultList)
    	 val statsP = addAssetsToPath(resultList(0))
    	   val permReportP = addAssetsToPath(resultList(1))
    	 val securityP =  addAssetsToPath(resultList(2)) 
    	 
    	 val heatMapReport = addAssetsToPath(resultList(3))
    	 val svgP = addAssetsToPath(resultList(4)) 
    	 
     
    	 println("statics Paths Server: ", statsP)
    	 println("graph Paths Server:", svgP)
    	  println("perm report Paths Server3453534534:", permReportP)
     
    	 Json.toJson(
    			 Seq(Json.toJson(
    					 Map("link"->Json.toJson(statsP),  "type" -> Json.toJson("Analysis Statistics")) 
    			 	),
    			 	Json.toJson(
    			 			Map("link"->Json.toJson(permReportP),  "type" -> Json.toJson(" Permission Report")) 
    			 	)
    			 	,
    			 	Json.toJson(
    			 			Map("link"->Json.toJson(securityP),  "type" -> Json.toJson(" Security Report in text")) 
    			 	),
    			 	Json.toJson(
    			 			Map("link"->Json.toJson(heatMapReport),  "type" -> Json.toJson(" Heat Map")) 
    			 	), 
    			 	Json.toJson(
    			 			Map("link"->Json.toJson(svgP),  "type" -> Json.toJson("Analysis State Graph")) 
    			 	)
    			 )
    		 )
   
  }*/
   
   def parseProperties(str: String) : Set[String] ={
     val l = str.length()
     if(l!=0){
        val subStr = str.substring(1,l)
        if(subStr.isEmpty()){
          Set[String]()
        }else{
           subStr.split("\\|").toList.toSet
        }
     }
     else Set[String]()
    
   }
   case class HeatPair(cnt : Int) {
     var percentil = 0.0
   }
   
  def computeRiskForCates(cates: Set[String]) : Int = {
     val rrm = Thread.currentThread().asInstanceOf[AnalysisHelperThread].riskRankingMap 
       var rankingSum = 0 
       cates.foreach(e => {
         val ro = rrm.get(e)
         ro match {
           case Some(n) => {
             rankingSum = rankingSum + n
           }
           case _ => { 
             
           }
         }
         
       })
       rankingSum
  }
   
      
   
  
 

   
    
   
   
  

  
}
    
