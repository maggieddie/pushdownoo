package org.ucombinator.dalvik.preanalysis
import org.ucombinator.dalvik.syntax._
import org.ucombinator.dalvik.vmrelated.DalvikVMRelated
import org.ucombinator.utils.CommonUtils
import scala.collection.immutable.Map
import scala.collection.mutable.{Map => MMap}
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.cfa.cesk.StmtForEqual
import org.ucombinator.dalvik.syntax.StmtNil
import org.ucombinator.playhelpers.AnalysisHelperThread

trait LiveRegisterAnalysis extends DalvikVMRelated with StmtForEqual{

   def initLiveSet(allStmts: List[Stmt]) : Map[StForEqual, Set[String]] = {
     allStmts.foldLeft(Map[StForEqual, Set[String]]())((res, st) => {
       val stStr = buildStForEqual(st) //CommonUtils.constrDistinctStatementStr(st)
       res + (stStr -> Set())
     })
  }
  
  /**
   * except for the if branches, and the pcakswitch have several successors.
   * well, and the goto stmt, it has the successors. successor:
   * here we don't escape line stmt, label stmt, and nop stmt for consistency
   * 
   *  Be careful that I could miss successors of other instructions: 
   *  one that I can think of is the return statmeent, the sucessor
   *  should be caller's next stmt! But the preanalysis, we don't know yet. Anyway
   *  
   * 
   */
  
  private def getSuccessorsLiveRegs (allStmts: List[Stmt], curS: Stmt, workMap: Map[StForEqual, Set[String]]) : Set[String] = {
    curS match {
      case ifS @ IfStmt(cond, sucLabel, nxt, ls, clsP, methP) => {
        val curN = ifS.next
      // val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        val nextStOption = Stmt.forLabel(sucLabel)
        val nextLblSt =
          nextStOption match {
            case Some(nextst) => nextst
            case None => throw new LRAException("If sucess label stmt unfound in Label Table!" + ifS.toString())
          }
         val nextSt = nextLblSt.next
         
         val curNStr = buildStForEqual(curN) // CommonUtils.constrDistinctStatementStr(curN)
         val nextStStr =buildStForEqual(nextSt)  //CommonUtils.constrDistinctStatementStr(nextSt)
        workMap(curNStr) ++ workMap(nextStStr)
      }
      case ss@SwitchStmt(testReg, offset, labls, nxt, ls, clsP, meP) => {
        val (nextStmtsE, nextStmtsNE) = ss.getBranchStmts
        
        nextStmtsNE.flatten((st) => {
          val stStr = buildStForEqual(st) //CommonUtils.constrDistinctStatementStr(st)
          workMap(stStr)
        }).toSet
      }
      
      case goS@GotoStmt(_, _, _,_,_) => {
        val lblStr = goS.label
        val nextStOption = Stmt.forLabel(lblStr)
         val nextLblSt =
          nextStOption match {
            case Some(nextst) => nextst
            case None => throw new LRAException("If sucess goto stmt unfound in Label Table!" + goS.toString())
          }
         val nextSt = nextLblSt.next
        val nextStStr =  buildStForEqual(nextSt) //CommonUtils.constrDistinctStatementStr(nextSt)
         workMap(nextStStr)
      }
      
      // every method stmt list ends with StmtNil
     //  and so the sucessors' live variable is empty set.
      case StmtNil => {
      
       // val stStr = buildStForEqual(curS) // CommonUtils.constrDistinctStatementStr(st)
        	 	//val res = workMap(stStr) // 
        	 //res
         Set()
      }
      
      case _ => {
       // println ("suLLLL: ", curS)
        	 	val st = curS.next//allStmts(indexOfCur+1)
        	 //	println("the next in the normal case: ", st)
        	 	
        	 	if(st==StmtNil) Set()
        	 	else {
        	 	val stStr = buildStForEqual(st) // CommonUtils.constrDistinctStatementStr(st)
       
        	 	//workMap.foreach(println)
        	 	val res = workMap(stStr)
        //	 	println("-----")
        	 	//println("the returning succ is: " , res)
        	 	res
        	 	}
         
      }
    }
  }
  
  /**
   * The LRA equation
   */
  private def applyLVAEquation(succRegs: Set[String], refsRegs: Set[String], defsRegs: Set[String]) : Set[String] = {
    succRegs diff defsRegs union refsRegs
  }

  // the code is subject to change
  // will this cause stack overflow?
  private def iterativeLRA(allStmts: List[Stmt], workMap: Map[StForEqual, Set[String]]) : Map[StForEqual, Set[String]] = {
   
    val localWorkMap: Map[StForEqual, Set[String]] = //= initLiveSet(allStmts)
      allStmts.foldLeft(Map[StForEqual, Set[String]]())((res, st) => {

       
        // get the live information from the last map
        val succLiveRegs = getSuccessorsLiveRegs(allStmts, st, workMap)

        val refSet = st.refRegsStrSet
        val defSet = st.defRegsStrSet
         // println("the the current Stmet: ", st)
        val newLiveRegsofCurSt = applyLVAEquation(succLiveRegs, refSet, defSet)
      
       // println("$" + succLiveRegs + " diff " + refSet + " union " + defSet)
       // println("res is: ", newLiveRegsofCurSt )
        val stStr = buildStForEqual(st) //CommonUtils.constrDistinctStatementStr(st)
        res + (stStr -> newLiveRegsofCurSt)
      //   res + (st -> newLiveRegsofCurSt)
      })
      
      // this equality should work, 
      if(localWorkMap == workMap) localWorkMap
      else 
        iterativeLRA(allStmts, localWorkMap)
  }
  
  /**
   * The entry to this kind of preanalysis
   * 
   * it will update the Stmt.liveMap
   */
  
  def runLRA(linkedHeadSt: Stmt ) :   (String, Stmt) = {
    
    val stLinkedList = CommonUtils.flattenLinkedStmt(List())(linkedHeadSt)
     
    
    val initialLRAMap = initLiveSet(stLinkedList)
    
    val fixPoint = iterativeLRA(stLinkedList, initialLRAMap)
    
    Stmt.extendLiveMap( fixPoint)
    
    (Stmt.toString(), linkedHeadSt)
    
  }
  
  def runLRAOnListSts(lst: List[Stmt]) {
    val initialLRAMap = initLiveSet(lst)
   
    val fixPoint = iterativeLRA(lst, initialLRAMap) 
    Stmt.extendLiveMap( fixPoint)
    
  }
  
  private def extendInitEntryListMap(res: Map[Stmt, List[Stmt]], k: Stmt, v: List[Stmt]) : Map[Stmt, List[Stmt]] = {
    val oldvs  = res.getOrElse(k, List())
    val newvs = oldvs ::: v
    res + (k -> newvs)
  }
  
  // precondition: the very first should be initentrypointe and the initSt is the same as the first hd
  private def getMapOfInitWithItsEntriesHelper(res: Map[Stmt, List[Stmt]])(lst: List[Stmt], initSt: Stmt) : Map[Stmt, List[Stmt]] = {
    lst match {
      case hd :: tl =>   {
        hd match {
          case StmtNil => res
         case InitEntryPointStmt(methodPath , argsTypes , body , regsNum ,nxt , ln, _,_ ) => {
          val newRes = extendInitEntryListMap(res, hd, List())
          getMapOfInitWithItsEntriesHelper( newRes)(tl, hd)
         }
          case  ei@EntryPointInvokeStmt(entr ,objRegStr , nxt , ln , clsP, methP) => {
                val newRes = extendInitEntryListMap(res, initSt, List(ei))
                 getMapOfInitWithItsEntriesHelper( newRes)(tl, initSt)
          }
        }
      }
      case Nil => {
        res
      }
    }
    
  }
  
  private def getMapOfInitWithItsEntries(entrySt: Stmt) : Map[Stmt, List[Stmt]] = {
     val stLinkedList = CommonUtils.flattenLinkedStmt(List())(entrySt)
     // skip the nop stmt
     val workList =   stLinkedList.head match{
       case NopStmt(_, _, _, _) | StmtNil => {
         /**
          * no class constructor
          */
         val headStr = buildStForEqual(stLinkedList.head)  //  CommonUtils.constrDistinctStatementStr(stLinkedList.head)
         Stmt.extendLiveMap(Map(headStr -> Set()))
         
         stLinkedList.drop(1)
       }
       case _ => {
         stLinkedList
       }
     }
     getMapOfInitWithItsEntriesHelper(Map[Stmt, List[Stmt]]())(workList, workList.head)
     
  }
  

  // for each chained entry points, we flatten and then run lra
  def runLRAEntryBodies (entrySt: Stmt )   :   (String, Stmt) = {
    
    val stLinkedList = CommonUtils.flattenLinkedStmt(List())(entrySt)
    
    val initWithBodyAndEntries = 
    stLinkedList.flatten{
      case s@StmtNil => List(s)
      case ns@NopStmt(_,_,_,_)  => {
        List(ns)
      }
      case ips@InitEntryPointStmt(_, _, _, _, _,_,_,_) => {
        val bd = ips.body
        val initbodyList = CommonUtils.flattenLinkedStmt(List())(bd)
        ips :: initbodyList
      }
      case eps@EntryPointInvokeStmt(_, _, _,_,_,_) => {
        val enbd = eps.entr.body
        val enBodyList = CommonUtils.flattenLinkedStmt(List())(enbd)
        eps :: enBodyList
      }
    }
    runLRAOnListSts(initWithBodyAndEntries)
    
    //runLRAOnListSts(stLinkedList)
/*    val mapOfIEies = getMapOfInitWithItsEntries(entrySt)
    
    mapOfIEies.foreach {
      case (k, vs) => {
        println("InitEntry: ", k)
        println("   Entries: ")
        vs.foreach(println)
      }
    }
    
    // just run nop
   
    mapOfIEies.foreach {
      case (initEntry, entries) =>{
        
        val initBody = initEntry.asInstanceOf[InitEntryPointStmt].body
        val initbodyList = CommonUtils.flattenLinkedStmt(List())(initBody)
        
        val entriesBody = entries.foldLeft(List[Stmt]())((res, entrSt) => {
          val enBody = entrSt.asInstanceOf[EntryPointInvokeStmt].entr.body
          val enBodyList = CommonUtils.flattenLinkedStmt(List())(enBody)
          res ::: (entrSt :: enBodyList)
        }) 
        
        val asOne = initEntry :: initbodyList ::: entriesBody
      
        println("the as one is")
        asOne.foreach(println)
        runLRAOnListSts(asOne)
      }
  }*/
   // Stmt.liveMap.foreach(println)
    (entrySt.toString(), entrySt)
  }
  
  
  
  /**
   * run on the Dalvik Class def all methods interaprocedually
   */
  
  def runLRAOnAllMethods {
     Thread.currentThread().asInstanceOf[AnalysisHelperThread].classTable.foreach {
       case (k, classDef) => {
         val methList = classDef.methods
         val nonEntryMethods = methList filter (! _.isEntryPoint)
         nonEntryMethods.foreach((methDef) =>{
           val methBody = methDef.body
           
           runLRA(methBody)
         })
       }
     }
  }
  
  class LRAException(str: String) extends Exception
  
  
}