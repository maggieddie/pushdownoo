package org.ucombinator.utils
import org.ucombinator.dalvik.syntax._
import scala.util.Random




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
      case ae@RegisterExp(_) => { Set(ae.regStr)}
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
   
    
   
   
  

  
}
    
