package org.ucombinator.dalvik.syntax

import scala.collection.immutable.{ Set => ImmSet, Map => ImmMap}
import scala.collection.mutable.Map
import org.ucombinator.utils.StringUtils
import org.ucombinator.utils.CommonUtils
import org.ucombinator.utils.Debug
import org.ucombinator.dalvik.informationflow.DalInformationFlow
import org.ucombinator.playhelpers.AnalysisHelperThread




sealed abstract class AExp

case class BooleanExp(sb: SBoolean) extends AExp {
  def value = sb.toString()
}

case class StringLitExp(ss: SText) extends AExp {
  def strLit = ss.value
}

case class CharLitExp(sc: SChar) extends AExp {
  def cha = sc
}

case class VoidExp() extends AExp {
}

//TODO
case class ThisExp() extends AExp{}

case class NullExp() extends AExp{}

case class RegisterExp(sv: SName) extends AExp {
  def regStr = sv.toString
  
  override def toString = regStr

}

case class IntExp(si: SInt) extends AExp {
  def numVal = StringUtils.strToInt(si.toString())
}

case class AutomicOpExp(opStr: SName, aes: AExp*) extends AExp {
  def sopCode = opStr
  def ops = aes
  
  override def toString = 
    StringUtils.truncateIfLong("AutomicOpExp" + "(" + opStr + "," + aes, 100)
}

case class InstanceofExp() extends AExp

abstract sealed class FieldExp( fp : String, ft: String) extends AExp{
 // def objExp : AExp =  oe
  def fieldPath : String = fp
  def fieldType: String = ft
  //def modifier: String = modi
}
case class StaticFieldExp ( fp : String, ft: String) extends FieldExp(fp, ft) {}
case class NonStaticFieldExp(or: AExp, fp : String, ft: String) extends FieldExp(fp, ft){
  def objExp : AExp = or
} 

// not used right now...
case class DalvikRefVar(val name: SName) extends AExp {
  override def toString = name.toString

  def isDuplicable = true

  lazy val mustReturnOrFail = true
  lazy val mustReturnUnspecified = false
  lazy val mayMutate: Boolean = false
  lazy val mayAllocate: Boolean = false
  lazy val mayPerformIO: Boolean = false

  lazy val free: ImmSet[SName] = ImmSet(name)
  lazy val keywords: ImmSet[SKeyword] = ImmSet()
  lazy val variables: ImmSet[SName] = ImmSet(name)
  lazy val mutables: ImmSet[SName] = ImmSet()
}

  case class StForEqual(oldStyleSt: Stmt, nextSt: Stmt, clsPath: String, methPath:String, lineSt: Stmt) {
    override def toString = oldStyleSt.toString
  }
  
abstract class Stmt  {
  def next: Stmt
  def next_=(stmt : Stmt )
  
  def lineNumber: Stmt 
  def lineNumber_=(stmt : Stmt )
  
  def clsPath: String
   def clsPath_=(str : String )
   
     def methPath: String
   def methPath_=(str : String )
  //def lineNumber: LineStmt

  /*protected def register(label: String) {
    Stmt.stmtMap += (label -> this)
  }
*/
   
  def refRegsStrSet : Set[String]  
  def defRegsStrSet: Set[String]
  
  /**
   * for security annotation parsed before feeding to analyzer
   * self inspection of the accessible information to decide 
   * whether the statement can be the source 
   * or the sink operation
   * 
   * This can be done during analysis
   * but it seems to be more clean doing here
   * 
   * 0 is clean, 1 for source, 2 for sink, 3 for both
   * 
   */
  def sourceOrSink: Int
  
  def taintKind: Set[String]
  
  def getTaintKind(name: String) : Set[String] = {
    DalInformationFlow.decideTaintKinds(name)
  }
  
  
  def decideInformationFlowType (clsNameOrAPI: String) : Int = {
    DalInformationFlow.decideSourceOrSinkLevel(clsNameOrAPI)
     /*val isSensitiveStringClsNameSource = DalInformationFlow.sources.map(_._1).contains(clsNameOrAPI)
     // should not be sink, well, this can be the java/io/OutputStream object
     
     val isSensitiveStringClsNameSink = DalInformationFlow.sinks.map(_._1).contains(clsNameOrAPI)
      
     if(isSensitiveStringClsNameSource && isSensitiveStringClsNameSink)  3 //both
     else if(isSensitiveStringClsNameSource == true && isSensitiveStringClsNameSink == false ) 1 //source
     else if (isSensitiveStringClsNameSource == false && isSensitiveStringClsNameSink == true ) 2 //sink
     else 0 //clean
*/  }   
  
}

object Stmt {
  
  // val stmtMap: Map[String, LabelStmt] = Map.empty
 // def register(label: String, lst: LabelStmt) {
  //  Stmt.stmtMap += (label -> lst)}
   
  //var liveMap : ImmMap[StForEqual, Set[String]] = ImmMap()
  
  def forLabel(label: String) = Thread.currentThread().asInstanceOf[AnalysisHelperThread].stmtMap.get(label)
  
  def updateLabelWith(label: String, newLblSt : LabelStmt) = 
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].stmtMap   +=  (label ->newLblSt)

  def extendLiveMap(newEntries: ImmMap[StForEqual, Set[String]]) {
     Thread.currentThread().asInstanceOf[AnalysisHelperThread].liveMap = Thread.currentThread().asInstanceOf[AnalysisHelperThread].liveMap ++ newEntries
   }
}

case class LabelStmt(lbl: String, nxt: Stmt, lineStmt: Stmt, clsP : String, methP : String) extends Stmt {
  def label: String = lbl
  var next: Stmt = nxt
  var lineNumber: Stmt = lineStmt
  
  var methPath = methP
  var clsPath = clsP
  
   def register(label: String) {
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].stmtMap += (label -> this)}
  //Stmt.register(lbl, this)
  
  override def toString() = "LabelStmt(" + lbl + "," +   ")" + lineNumber
  
  def refRegsStrSet : Set[String] = {
    Set()
  }
  
  def defRegsStrSet: Set[String] ={Set()}
  
  def sourceOrSink = 0
  def taintKind = Set()
}

case class LineStmt(lnstr : String, nxt: Stmt, ln: Stmt, clsP : String, methP : String) extends Stmt  {
  def linenumber = lnstr
  var next: Stmt = nxt
    var lineNumber = ln
    override def toString() = "LineStmt(" + lnstr + ")" 
     def refRegsStrSet : Set[String] = {
    Set()
  }
    var methPath = methP
  var clsPath = clsP
  
  def defRegsStrSet: Set[String] ={Set()}
    
    def sourceOrSink = 0
    def taintKind = Set()
}

case class NopStmt(nxt: Stmt, ln: Stmt, clsP: String, methP: String) extends Stmt   {
	var next = nxt
	var lineNumber = ln
	override def toString() = "NopStmt(" +  ")" + lineNumber
	def refRegsStrSet: Set[String] = {
		Set()
	}
	var methPath = methP
	var clsPath = clsP

	def defRegsStrSet: Set[String] = { Set() }

	def sourceOrSink = 0
	def taintKind = Set()
}

case class GotoStmt(lbl: String, nxt: Stmt, ln: Stmt, clsPPath: String, methPPath: String) extends Stmt   {
  def label = lbl
  var next = nxt
  var lineNumber = ln
  override def toString() = "GotoStmt(" + lbl + "," +  ")" +  StringUtils.stmtContextInfo(clsPPath, methPPath, lineNumber)
  def refRegsStrSet: Set[String] = {
    Set()
  }
    var methPath = this.methPPath
  var clsPath = clsPPath

  def defRegsStrSet: Set[String] = { Set() }
    
  def sourceOrSink = 0
  def taintKind = Set[String]()
}

// 
 object StmtNil  extends Stmt {
  def next = throw new Exception("no more stmt")
  def next_=(st: Stmt) { throw new ParsingExpcetion("can't change next on null stmt") }
  def lineNumber = throw new ParsingExpcetion("no line stmt")
  def lineNumber_=(st: Stmt) { throw new ParsingExpcetion("can't change linenumber on null stmt") }
  
    def methPath = ""
      def methPath_=(st: String) {}
    def clsPath = ""
      def clsPath_=(st: String) {}
  
  def refRegsStrSet: Set[String] = {
    Set()
  }

  def defRegsStrSet: Set[String] = { Set() }

  def sourceOrSink = 0
  def taintKind = Set()
}


case class NewStmt(destReg: SName ,clsName: String, nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  def destRegister = new RegisterExp(destReg)
  def classPath = clsName
  
  var next = nxt
  var lineNumber = ls
  
  var clsPath = clsP
  var methPath = methP
  
  override def toString() = "NewStmt(" + destReg + ","+ clsName +  ")"  +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
  def getFieldStrs (clsName: String) : List[(String,String)] = {
    val clsDefO = DalvikClassDef.forName(clsName)
    Debug.prntDebugInfo("The New Stmt's class: ", clsDefO)
    clsDefO match {
      case Some(cd) => {
        cd.getAllFields
      }
      case None => {
        List()
      }
    }
  }
  
   def refRegsStrSet : Set[String] = {
    Set()
  }
  
   def defRegsStrSet: Set[String] ={
    Set(destReg.toString())
   }
   
   /**
    * self-inspect the thing to new
    */
   
   def sourceOrSink : Int ={
     decideInformationFlowType(clsName)
   }
   
   def taintKind =  getTaintKind(clsName)
}

case class IfStmt(condExp: AExp, sucLabel: String, nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  var next = nxt
  var lineNumber = ls
  def condition = condExp
  def succLabel = sucLabel
    var clsPath = clsP
  var methPath = methP
  override def toString() = "IfStmt(" + condExp + "," + sucLabel  + ")" +    StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
  def refRegsStrSet: Set[String] = {
    condExp match {
      case re@RegisterExp(_) => {
        Set( re.regStr)
      }
      
      case aoe@AutomicOpExp(if2,  aExps @ _*) => {
        val operatorAes = aoe.ops.toList
        CommonUtils.getRegStrsListFromAExpList(operatorAes)
      }
    }
  }

  def defRegsStrSet: Set[String] = {
    Set()
  }
  
  // nothing interesting here
  def sourceOrSink  = 0
   def taintKind = Set()
}

case class SwitchStmt(testExp: AExp, offset: String, lables: List[AExp],   nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  var next  = nxt
  var lineNumber = ls
    var clsPath = clsP
  var methPath = methP
  override def toString() = "Packed/SparseSwitch(" + testExp + "," + offset +  ")"  +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
  /**
   * with the label, nop and StmtNil Line stmt escaped and the one not escaped for convinience
   */
 def getBranchStmts : (List[Stmt], List[Stmt]) = {
	 
        val sExps = 
           lables filter ((lba) => {
        	   	lba match {
        	   		case sle@StringLitExp(_) => true 
            	 	case _ => false
          	 }
           })
        
         val strExps = sExps map (_.asInstanceOf[StringLitExp])
         val labelStrs = strExps map (_.strLit)
          Debug.prntDebugInfo(" the table ", Thread.currentThread().asInstanceOf[AnalysisHelperThread].stmtMap)
         val stmtOs0 = labelStrs.map ((ls) => {
            
             Stmt.forLabel(ls)
         })
         
         
         val stmtOs = lables.foldLeft(List[Option[Stmt]]())((res, lblAe) => {
           lblAe match {
             case sle@StringLitExp(_) =>  {
               val resS = sle.strLit
               Stmt.forLabel(resS) :: res
             }
             case re@RegisterExp(_) => {
                Stmt.forLabel(re.regStr) :: res
             }
             case _ => res 
           }
         })
         
         
      val stmtS=  stmtOs filter ((os : Option[Stmt]) => { 
           os match {
           case Some(s) => true
           case None => false
           }
         })
        
        val stmtNoE =  stmtS map ((s) => {
           s match {
             case Some(s) => s.next
           }
         })
        
         val stmtE =  stmtS map ((s) => {
           s match {
             case Some(s) => CommonUtils.findNextStmtNotLineOrLabel(s.next)
           }
         })
        (stmtE, stmtNoE)
  }

  
   def refRegsStrSet : Set[String] = {
    testExp match {
      case ae@RegisterExp(_) => {
        Set(ae.regStr)
      }
    }
  }
  
   def defRegsStrSet: Set[String] ={
    Set()
   }
   
    // nothing interesting here
  def sourceOrSink  = 0
  def taintKind = Set[String]()
}

case class AssignAExpStmt(lhReg: AExp, rhExp: AExp, nxt: Stmt, ls : Stmt, clsP: String, methP: String) extends Stmt {
  var next = nxt
  var lineNumber = ls
   var clsPath = clsP
  var methPath = methP
  
  def rhs = rhExp
  def lhs = lhReg
  
  
  override def toString = "AssignAExpStmt(" + 
      lhReg + "," + rhExp  + ")" +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
  def isConstString : (Boolean, List[AExp]) = {
      import CommonSSymbols._;
    rhExp match {
      case AutomicOpExp(opCode, aExps @ _*) => {
        opCode match{
          case SConstString => (true, aExps.toList)
          case _ => (false, aExps.toList)
        }
      }
      case _ => (false, List())
    }
  }
   

  private def getSuitableRefStrsSet: Set[String] = {
    rhExp match {
      case re@RegisterExp(sv) => { // should be move-result
        Set(re.regStr)
      }
      case AutomicOpExp(opCode, aExps @ _*) => {
        CommonUtils.getRegStrsListFromAExpList(aExps.toList)
      }
      // otherwise, it will be in the aExps
      case _ => throw new Exception("@getSuitableRefStrsSet: ASsignAexpStmt: right hand side can't be RegisterExp and AutomicOpExp" + rhExp.toString())
    }
  }
  
  
  def refRegsStrSet : Set[String] = {
    getSuitableRefStrsSet
  }
  
   def defRegsStrSet: Set[String] ={
     CommonUtils.getRegStrsFromAExp(lhReg)
   }
   
   /**
    * the strings are from sensitive string configuration
    * or some url
    */
   def sourceOrSink : Int = {
     val (isConstStr, aExps) = isConstString
    
     if(isConstStr){
        if (aExps.length == 1) {
          aExps.head match { 
            case se @ StringLitExp(_) => {
             val   constStr = se.strLit
             if(DalInformationFlow.isSensitiveStr(constStr))
               1 
               else 0
            }
            case _ => 0
          }
        }
        else throw new Exception("@sourceOrSink@AssignmentStmt: string statement has more than aexp: " + this)
     }
     else 0
   }
   
   
   def taintKind  = {
     val (isConstStr, aExps) = isConstString
    
     if(isConstStr){
        if (aExps.length == 1) {
          aExps.head match { 
            case se @ StringLitExp(_) => {
              DalInformationFlow.getTaintKindsForString(se.strLit)
            }
            case _ => Set[String]()
          }
        }
        else throw new Exception("@sourceOrSink@AssignmentStmt: string statement has more than aexp: " + this)
     }
     else Set[String]()
   }

}

// isn;t like the assignAExp? but simplify to match anyway
case class FieldAssignStmt(lhr: AExp, fe: AExp, nxt: Stmt, ls: Stmt , clsP: String, methP: String) extends Stmt {
  def lhs = lhr
  def rhs = fe
  var next = nxt
  var lineNumber = ls
    var clsPath = clsP
  var methPath = methP
   override def toString = "FieldAssignStmt(" + lhr + "," + fe  +")"  +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)

  /* the reason that we can't write the applyFieldAssign here is that the front end does not know what types of the analyzer.
    * like the Store, FramePointer, or KAddr
    */

  // just for registers, not for specific field. 
  //but we can do that

  def refRegsStrSet: Set[String] = {
    fe match {
      case nonStFE @ NonStaticFieldExp(or, fp, ft) => {
        lhr match { // iget
          case re @ RegisterExp(_) => {
            val res = CommonUtils.getRegStrsFromAExp(or)
          
            res 
          }
          case _ => { throw new Exception("@refRegsStrSet: cant be anything else") }
        }
      }
      //sget
      case sfe @ StaticFieldExp(fp, ft) => {
        lhr match { // sget
          case re @ RegisterExp(_) => {
            Set()
          }
          case _ => { throw new Exception("@refRegsStrSet: cant be anything else") }
        }
      }
      // iput or sput
      case re @ RegisterExp(_) => {
        lhr match {
          case nonStFE @ NonStaticFieldExp(or, fp, ft) => {
            CommonUtils.getRegStrsFromAExp(or) ++
              CommonUtils.getRegStrsFromAExp(fe)
          }
          //sput
          case sfe @ StaticFieldExp(fp, ft) => {
            CommonUtils.getRegStrsFromAExp(fe)
          }
        }
      }
    }
  }

  def defRegsStrSet: Set[String] = {
    fe match {
      // iget
      case nonStFE @ NonStaticFieldExp(or, fp, ft) => {
        CommonUtils.getRegStrsFromAExp(lhr)
      }
      //sget
      case sfe @ StaticFieldExp(fp, ft) => {
        CommonUtils.getRegStrsFromAExp(lhr)
      }
      
        // iput or sput
      case re @ RegisterExp(_) => {
        lhr match {
          case nonStFE @ NonStaticFieldExp(or, fp, ft) => {
             Set()
          }
          //sput
          case sfe @ StaticFieldExp(fp, ft) => {
             Set()
          }
        }
      }
    } 
  }
  
  /**
   * security related
   */
  
   // nothing interesting here, for now
  def sourceOrSink   = {
    fe match {
         case sfe @ StaticFieldExp(fp, ft) => {
        lhr match { // sget
          case re @ RegisterExp(_) => {
             this.decideInformationFlowType(sfe.fp)
          }
          case _ => { throw new Exception("@refRegsStrSet: cant be anything else") }
        }
      }
         case _ => 0
     }
  } 
  
 
  def taintKind = {
     fe match {
         case sfe @ StaticFieldExp(fp, ft) => {
        lhr match { // sget
          case re @ RegisterExp(_) => {
             this.getTaintKind(sfe.fp)
          }
          case _ => { throw new Exception("@refRegsStrSet: cant be anything else") }
        }
      }
         case _ => Set[String]()
     }
  }//Set[String]()
}

abstract sealed class AbstractInvokeStmt(methPathStr: String, argRA: List[AExp], tyStrs: List[String], clsP: String, methP: String) extends Stmt {
  def methPPath: String = methPathStr
  def argumentRegAExp: List[AExp] = argRA
  def argTypes: List[String] = tyStrs
  
  //apply method goes in there. But I don't like the analyzer types to be mixed in the front end here.
}

// invoke direct and invoke virtual
case class InvokeStmt(methPathStr: String, argRegAExp: List[AExp], objAExp: AExp, tyStrs: List[String], nxt: Stmt, ls:Stmt, clsP: String, methP: String)
  extends AbstractInvokeStmt(methPathStr, argRegAExp, tyStrs, clsP, methP) {
  var next = nxt
  var lineNumber = ls
    var clsPath = clsP
  var methPath = methP
  def objectAExp = objAExp
  override def toString = "InvokeStmt:" + methPathStr + "("+ argRegAExp + " , "+ objAExp + " , "+ 
  tyStrs +  ")"  +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
  def refRegsStrSet: Set[String] = {
    CommonUtils.getRegStrsListFromAExpList(objAExp :: argRegAExp)
  }

  def defRegsStrSet: Set[String] = { Set() }
  
  
  def sourceOrSink  = decideInformationFlowType(methPathStr)
  def taintKind = {
    
    val res  = getTaintKind(methPathStr)
   
    res
  }
}

case class InvokeSuperStmt(methPathStr: String, argRegAExp: List[AExp], objAExp: AExp, tyStrs: List[String], nxt: Stmt, ls:Stmt, clsP: String, methP: String)
  extends AbstractInvokeStmt(methPathStr, argRegAExp, tyStrs, clsP, methP) {
  var next = nxt
  var lineNumber = ls
      var clsPath = clsP
  var methPath = methP
  def objectAExp = objAExp
  override def toString = "InvokeSuperStmt: " + methPathStr + "("+ argRegAExp + " , "+ objAExp + " , "+ tyStrs +   ")"  +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
  def refRegsStrSet: Set[String] = {
    CommonUtils.getRegStrsListFromAExpList(objAExp :: argRegAExp)
  }

  def defRegsStrSet: Set[String] = { Set() }
    def sourceOrSink  = decideInformationFlowType(methPathStr)
    def taintKind = getTaintKind(methPathStr)
}

case class InvokeInterfaceStmt(methPathStr: String, argRegAExp: List[AExp], objAExp: AExp, tyStrs: List[String], nxt: Stmt, ls:Stmt, clsP: String, methP: String)
  extends AbstractInvokeStmt(methPathStr, argRegAExp, tyStrs, clsP, methP) {
  var next = nxt
  var lineNumber = ls
      var clsPath = clsP
  var methPath = methP
  def objectAExp = objAExp
  override def toString = "InvokeInterfaceStmt:"   + methPathStr + "("+ argRegAExp + " , "+ objAExp + " , "+ tyStrs +   ")"  +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
  def refRegsStrSet: Set[String] = {
    CommonUtils.getRegStrsListFromAExpList(objAExp :: argRegAExp)
  }

  def defRegsStrSet: Set[String] = { Set() }
   def sourceOrSink  = decideInformationFlowType(methPathStr)
   def taintKind = getTaintKind(methPathStr)
}

// invoke static
case class InvokeStaticStmt(methPathStr: String, argRegAExp: List[AExp], tyStrs: List[String], nxt: Stmt, ls:Stmt, clsP: String, methP: String)
  extends AbstractInvokeStmt(methPathStr, argRegAExp, tyStrs, clsP, methP) {
  var next = nxt
 // def objectAExp = objAExp
  var lineNumber = ls
      var clsPath = clsP
  var methPath = methP
   override def toString = "InvokeStaticStmt:"+ methPathStr + "(" + argRegAExp  +  " " +  tyStrs +   ")"  +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)
   def refRegsStrSet: Set[String] = {
    CommonUtils.getRegStrsListFromAExpList( argRegAExp)
  }

  def defRegsStrSet: Set[String] = { Set() }
    def sourceOrSink  = decideInformationFlowType(methPathStr)
    def taintKind = getTaintKind(methPathStr)
}

case class InvokeDirectStmt(methPathStr: String, argRegAExp: List[AExp], objAExp: AExp, tyStrs: List[String], nxt: Stmt, ls:Stmt, clsP: String, methP: String)
  extends AbstractInvokeStmt(methPathStr, argRegAExp, tyStrs, clsP, methP) {
  var next = nxt
  var lineNumber = ls
      var clsPath = clsP
  var methPath = methP
  def objectAExp = objAExp
  override def toString = "InvokeDirectStmt: " + methPathStr + " ( "+ objAExp  + " , "+ argRegAExp + ","+ tyStrs + ")" + 
   StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
   def refRegsStrSet: Set[String] = {
    CommonUtils.getRegStrsListFromAExpList(objAExp :: argRegAExp)   
  }

  def defRegsStrSet: Set[String] = { Set() }
   def sourceOrSink  = decideInformationFlowType(methPathStr)
   def taintKind = getTaintKind(methPathStr)

}

case class ReturnStmt(resultAe: AExp, nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  var next = nxt
  def result = resultAe
  var lineNumber = ls
  
      var clsPath = clsP
  var methPath = methP
  override def toString = "Return:" + resultAe +  ")"  +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)

  def refRegsStrSet: Set[String] = {
    resultAe match {
      case ae @ RegisterExp(_) => {
        if (ae.regStr == "") Set()
        else Set(ae.regStr)
      }
      case _ => { throw new Exception(" exception from getRegStrsFromAExp: not a RegisterExp, Found:" + resultAe.toString) }
    }
  }

  def defRegsStrSet: Set[String] = { Set("ret") }
  
  def sourceOrSink  = 0
  def taintKind =  Set[String]()
}

// includes the object 
//case class FieldAssginment extends Stmt


// the exnHandler is supposed to be singletonList
case class PushHandlerStmt(typeString: String, clsName: String, lbl: String, timetoFork: Boolean, exnHandlers: List[ExceptionHandlers],   exnAnno: List[String], nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  var next = nxt
  def className = clsName
  def label = lbl
  var lineNumber = ls
  
      var clsPath = clsP
  var methPath = methP
  
  override def toString =  "PushHandlerStmt(" + clsName+ "," +lbl+ "," +  ")" + StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
    def refRegsStrSet: Set[String] = {
    Set()
  }

  def defRegsStrSet: Set[String] = { Set() }
  def sourceOrSink  = 0
   def taintKind =  Set[String]() 
}

case class PopHandlerStmt(exnType: String, nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  var next = nxt
  var lineNumber = ls
  override def toString =  "PopHandlerStmt(" + exnType +  ")" + StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
      var clsPath = clsP
  var methPath = methP
  
  def refRegsStrSet: Set[String] = {
    Set()
  }

  def defRegsStrSet: Set[String] = { Set() }
  def sourceOrSink  = 0
   def taintKind =  Set[String]()
}

case class FaultInjectorStmt(exnHandlers: ExceptionHandlers, exnAnnos: List[String], nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  var next = nxt
  var lineNumber = ls
  override def toString = "FaultInjectorStmt: " +  " " + StringUtils.stmtContextInfo(clsP, methP, lineNumber)

  def refRegsStrSet: Set[String] = {
    Set()
  }
  
      var clsPath = clsP
  var methPath = methP

  def defRegsStrSet: Set[String] = { Set() }
      def sourceOrSink  = 0
       def taintKind =  Set[String]()

}


case class ThrowStmt(exn: AExp, nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  var next = nxt
  var lineNumber = ls
  def exeption = exn
  override def toString =  "ThrowStmt(" + exn  +  ")" + StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
  
      var clsPath = clsP
  var methPath = methP
  def refRegsStrSet: Set[String] = {
    CommonUtils.getRegStrsFromAExp(exn)
  }
 
  def defRegsStrSet: Set[String] = { Set("exn") }
  def sourceOrSink  = 0
   def taintKind =  Set[String]()
}

case class MoveExceptionStmt(nameReg: AExp, nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  var next = nxt
  var lineNumber = ls
  def nameAexp = nameReg
  
      var clsPath = clsP
  var methPath = methP
  override def toString =  "MoveExceptionStmt(" + nameReg  +  ")" + StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  
  def refRegsStrSet: Set[String] = {
    Set("exn")
  }

  def defRegsStrSet: Set[String] = { 
    CommonUtils.getRegStrsFromAExp(nameReg)
  }
  def sourceOrSink  = 0
   def taintKind =  Set[String]()
}

case class CatchStmt(typeStr: String, exnTy: String, fromLblStr: String, toLblStr: String, usingLblStr: String, nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
  def exceptionType = exnTy
  def from = fromLblStr
  def to = toLblStr
  def using = usingLblStr
  var next = nxt
  var lineNumber = ls
      var clsPath = clsP
  var methPath = methP
  override def toString =  "CatchStmt(" + typeStr + ","+ exnTy + "," + fromLblStr  + "," + toLblStr  + ")" + 
  StringUtils.stmtContextInfo(clsP, methP, lineNumber)
  def refRegsStrSet: Set[String] = {
    Set()
  }

  def defRegsStrSet: Set[String] = { Set() }
  def sourceOrSink  = 0
   def taintKind =  Set[String]()
}

case class CompactMethodIndex(methPath: String, argsTypes: List[String]) {
  def methodPath = methPath
  def argTypes = argsTypes
  
   override def toString = methPath + " " + argsTypes.toString()
   override def hashCode = 41 *  methPath.hashCode() + argsTypes.hashCode()
   override def equals(other: Any) = other match {
      case that : CompactMethodIndex => //(that canEqual this) && 
      								(this.methodPath == that.methodPath) && (this.argTypes == that.argTypes)
      case _ => false
    }
   // override def canEqual(other: Any) = other.isInstanceOf[CompactMethodIndex]
   
   
}



case class DalvikClassDef(
    val clsPath: String, 
    superClsName: SName, 
    fields : List[FieldDef], 
    methods: List[MethodDef], 
    interfaceNames: List[String],
    ivt: Map[CompactMethodIndex, MethodDef]) {

  def className = clsPath.toString()

  def superName = superClsName.toString()
  def interfaceVirtualTable = ivt
 
  private def methodTable : Map [CompactMethodIndex, MethodDef] = {
    val entries: List[(CompactMethodIndex , MethodDef)] = methods map ((md:MethodDef) => 
      (new CompactMethodIndex(md.methodPath, md.argTypeList), md))
    val mt : Map[CompactMethodIndex, MethodDef] = Map.empty
    for((key, inst) <- entries) mt += (key->inst)
    mt
  }
  private def fieldTable: Map[String, FieldDef] = {
    val entries: List[(String , FieldDef)] = fields map ((fd:FieldDef) => (fd.fieldPath, fd))
    val mt : Map[String, FieldDef] = Map.empty
    for((key, inst) <- entries) mt += (key->inst)
    mt
  }
  
  def getFieldPathStrs () : List[String] = {
   this.fields map (_.fieldPath)
  }
  
  private def isClintMeth(md: MethodDef) : Boolean ={
      val attrs = md.attrs
      val methName = md.methodPath
      if(attrs.contains("static") && methName.contains("<clinit>") && (!attrs.contains("private")) ) true
      else false
    }
  
  // singleton if found
  def getClassConstrctorMethDef : List[MethodDef] ={
    this.methods filter isClintMeth
  }
  
  /**
   * Get all the inheritable fields
   */
  def getInheritableFieldPathTypeStrs : List[(String, String)] ={
    this.fields.foldLeft(List[(String, String)]()) ((res, fieldDef: FieldDef) => {
      if(fieldDef.attrs.contains("private") || fieldDef.attrs.contains("static") ){
        res
      }else {
        (fieldDef.fieldPath, fieldDef.fieldType) :: res
      }
    })
  }
  
  /*** 
   * to get the fields include all the inherited ones
   * BugFix: the fields in the curnet class shold not exclude private ones
   * */
  def getAllFields : List[(String, String)] ={
    val superdefs = getSupers(List())(this.superName);
    val allfields = // (this:: superdefs ) .foldLeft(Set[(String,String)]())((res, clsDef) => {
      superdefs.foldLeft(Set[(String,String)]())((res, clsDef) => {
      res ++ clsDef.getInheritableFieldPathTypeStrs.toSet // to avoid duplicate?
    })
     getCurrentInstanceFields ::: allfields.toList
  }
  
  def getCurrentInstanceFields: List[(String, String)] = {
    val all = this.fields
    val fall = all.filter(! _.attrs.contains("static"))
    fall.map((fd) => (fd.fieldPath, fd.fieldType))
  }
  
  private def addMethod(methDef : MethodDef) = {
    methodTable += (new CompactMethodIndex(methDef.methodPath, methDef.argTypeList) -> methDef)
  }
  
  private def addAllMethods(methLst : List[MethodDef])  {
      for(md <- methLst) addMethod(_)
  }
  
  private def addField(fldDef: FieldDef) ={
    fieldTable += (fldDef.fieldPath -> fldDef)
  }
  
  private def AddAllField(flds: List[FieldDef]) {
    for(fd <- flds) addField(_)
  }
  
  def lookupCurClsMethTable(compactMeth: CompactMethodIndex) : Option[MethodDef] ={
   
     val res = methodTable get compactMeth
     res
  }
  
  def lookupCurClsInterfaceTableCache(compactMeth: CompactMethodIndex): Option[MethodDef] ={
      Debug.prntDebugInfo("the current compactMehotdin in lookup", compactMeth)
     val res = ivt  get compactMeth
     res
  }
  
  def cacheFoundInterfaceDef(compactMeth: CompactMethodIndex, md: MethodDef) {
    ivt += (compactMeth -> md)
  }
  
  def getSuperClsDefO: Option[DalvikClassDef] = {
    DalvikClassDef.forName(this.superName)
  }

  // TODO: Test
  // TODO: get interfaces types too
  //  here we only returns supers defined 
  // 
  private def getSupers(res: List[DalvikClassDef])(curSuper: String): List[DalvikClassDef] = {
    val curSuperClsDefOpt = DalvikClassDef.forName(curSuper)
    curSuperClsDefOpt match {
      case None => res.reverse
      case Some(cscd) => {
        val nxtSuper = cscd.superName
        getSupers(cscd :: res)(nxtSuper)
      }
    }
  }
  
 /**
  *  get all the literal supers of current class
  */
  def getSupersStr(res: List[String])(curClass: String) : List[String] = {
      val curSuper = DalvikClassDef.getSuperOfCurCls(curClass)
    
      val curSuperClsDefOpt = DalvikClassDef.forName(curSuper)
    curSuperClsDefOpt match{ // if not found, then also add the cursuper str
      case None => curSuper :: res.reverse
      case Some(cscd) => {
        val nxtSuper = cscd.superName
        getSupersStr(curSuper :: res)(nxtSuper)
      }
    }
  }
  
  /**
   * since we don't parse interface class file, when getting the superstrs, we separate
   * the supers here
   */
  
  def getSuperStrsIncludingInterfaces(res: List[String])(curClass: String) : List[String] = {
    val classSupers = getSupersStr(res)(curClass)
    
      this.interfaceNames ::: classSupers 
    
  }
  
   def registerClass(clsName: String) {
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].classTable += (clsName -> this)
  }
   
   /**
    * This will be called for entry points
    * bugfix: the private init should not be explored individually
    */
   def getInitMethods : List[MethodDef] = {
      val initPath = StringUtils.getDistinctMethodOrFieldPath(this.className, "<init>", "meth")
     // this.methods.filter(_.methodPath == initPath)
      this.methods.filter((methDef) => {
        (methDef.methodPath == initPath) && (!methDef.attrs.contains("private"))
      })
      
   }
   override def toString = "\n\n"+ "CLASSNAME: "+ className + "\n" +superName + "\n" + "++ Field" + fields + "\n" + "++ Method: " + methods

}

/**
 * classes, (no interfaces)
 */
object DalvikClassDef {

  // easy access to the classes
  // val classTable: Map[String, DalvikClassDef] = Map.empty

  def forName(clsName: String)  = Thread.currentThread().asInstanceOf[AnalysisHelperThread].classTable.get(clsName)
  
  def getAllClassConstructors : List[MethodDef] = {
     Thread.currentThread().asInstanceOf[AnalysisHelperThread].classTable.foldLeft(List[MethodDef]())((res, kv)=>{
        val curClsDef = kv._2
        curClsDef.getClassConstrctorMethDef ::: res
     })
   }
   
     def getAllClInitStmts: List[Stmt] = {
     getAllClassConstructors.map(_.body)
     }
     
     def getAllClInitPaths: List[String] ={
       getAllClassConstructors.map(_.methodPath)
     }
     
     def isInterface(name: String) : Boolean = {
       /**
        * we only search through the entire class table
        */
     val values = Thread.currentThread().asInstanceOf[AnalysisHelperThread].classTable.values 
     val res = values.filter((clsDef) => {
       val ifs = clsDef.interfaceNames
       if(ifs.contains(name)) true else false
     })
     ! res.isEmpty
     }

  /**
   * ****
   * direct/super/virtual
   * if found, in currnet classDef, then return
   * otherwise, get the currentclasss super class.
   * and continue to find.
   * But, when to stop?
   * or we can compute the supers beforehand.
   * suoers are all the classes we can find in the app class table,
   * if we hit some class that's not defined in the current table,
   * we still return  None
   * but if for override method, we are going to find the method in
   * the subclass.
   * but for the function that we inherit from the parent class,
   * we are not gonna find it.
   * this maybe confused with that method undefined. we are not going to
   * find it neither.(of course)
   * In addition, it should return singleton list!
   */
  def resolveMethod(clsDef: DalvikClassDef, methIndex: CompactMethodIndex): List[MethodDef] = {
    val supers = clsDef.getSupers(List())(clsDef.superName)
    val allClsToExplore = supers
    val argStrs = methIndex.argsTypes
    val curObjCls = clsDef.className
    val startingMethIndex = StringUtils.buildNewMethodIndex(curObjCls, methIndex)
    
    val curMethDefOpt = clsDef.lookupCurClsMethTable(startingMethIndex)  // again for the current class
    curMethDefOpt match {
      case Some(md) => List(md)
      case None => { // continue lookup
        val resolvedTargets = allClsToExplore.foldLeft(List[MethodDef]())((res, cls) => {
          val newMethIndex = StringUtils.buildNewMethodIndex(cls.className, methIndex)
          val methodDefOpt = cls.lookupCurClsMethTable(newMethIndex)
          methodDefOpt match {
            case Some(md) => md :: res
            case None => res
          }
        })
        Debug.prntDebugInfo("Resolved Methods number for Meth" + methIndex.toString(), resolvedTargets.length)
        resolvedTargets.reverse
      }
    }
  }
  
  def getSuperOfCurCls(clsName: String) : String = {
    val curClassDefO = DalvikClassDef.forName(clsName)
     curClassDefO match {
        		case Some(curClsDef) => {
        		  curClsDef.superName
        		}
        		case None => "" }
  }
   
   /**
    * test whether the class specified by the first argument is subtype of second argument
    * we'll need to get the first class definition in the current system, 
    * then get all its supers.
    * if supers is none, depends on the first =? second
    * else, we will see whether all the classes of the first argument contains the second className
    * Problem: if the first argument not found the type in the Dalvik Def, we will need eto add the 
    * some default class types, for example, the java/lang/Exception, it is not in the Dalvik class
    * Definition!
    * 
    * Modification: just elimietes the type of itself. Since we can detect it directly
    */
    def isInstanceofParents(curCls : String,   otherClsName: String) : Boolean = {
      val curClasDefOp = DalvikClassDef.forName(curCls)
        curClasDefOp match {
        		case Some(curClsDef) => {
        			val curSupers =  curClsDef.getSupers(List())(curCls)
        			val curAllSupers =  (defaultClasses ::: curSupers)
        			  curAllSupers.contains(otherClsName)
        		}

        		case None => {
        		  Debug.prntDebugInfo("Dalvik isInstanceOf class Not found! " , curCls)
        		   defaultClasses.contains(otherClsName)
        		}
      }
    }
    
    def defaultClasses : List[String] = {
      List("java/lang/Exception", 
          "java/lang/ClassNotFoundException", 
          "java/lang/IOException",
          "java/lang/InterruptedException",
          "java/lang/NoSuchMethodException",
          // UNchecked
          "java/lang/IllegalArgumentException",
          "java/lang/ArrayIndexOutOfBoundsException",
          "java/lang/ArithmeticException",
          "java/lang/NullPointerException",
          "java/lang/NumberFormatException",
          "java/lang/RuntimeException"
      )
    }
    
  /**
   * This will first look up in the interface vritual table cache
   * if not found, then will do normal method resolution
   * If found, not only returns the result, but also update the interfae table
   */
    
    def lookUpInterfaceMethod(clsName: String, methPath: String, argTypes: List[String]): List[MethodDef] = {
       val methIndex = new CompactMethodIndex(methPath, argTypes)
         val methClassDefO = forName(clsName)
         methClassDefO match {
         case Some(d) => {
           d.lookupCurClsInterfaceTableCache(methIndex) match {
             case Some(md) => {
              List(md)
             }
          case None => { // go through the normal method resolution process 
            val methName = StringUtils.getMethNameFromMethPath(methPath)
            val actualMathPath = StringUtils.getDistinctMethodOrFieldPath(clsName, methName, "meth")
         
            val actualmethIndex = CompactMethodIndex(actualMathPath, argTypes)
            d.lookupCurClsMethTable(actualmethIndex) match {
              case Some(md) => {
                d.cacheFoundInterfaceDef(methIndex, md)
                List(md)
              }
              case None => {
                 val climRes = resolveMethod(d, actualmethIndex)
                climRes
              }
            }
          }
        }
      }
        case None => {
               Debug.prntDebugInfo("class not found for intreafx ", methIndex.toString())
               List()
            }
       }
    }
    
 
    
   private def lookupStaticOrDirect(MethClassDef: Option[DalvikClassDef], methIndex: CompactMethodIndex) : List[MethodDef] ={
     
     MethClassDef match {
        case Some(d) => {
          
          d.lookupCurClsMethTable(methIndex) match {
            case Some(md) => {
            
              List(md)
            }
            case None => {
             
             
              /**
               * since the invoke-direct will invoke super class's constructor on subclass's instance object
               * which made us can't find the superclass init path on the subclss's meta information
               * here we  switch to use class path directly maybe the following guaad don't need.
               */
            
               val objClsName = d.className
               val mathPath = methIndex.methodPath
               val methPathClsName = StringUtils.getClassPathFromMethPath(mathPath)
               val methName = StringUtils.getMethNameFromMethPath(mathPath)
               if(methName == "<init>" && objClsName != methPathClsName && methPathClsName!= "java/lang/Object"){
                // we'll just get the clas path the direct method
                 DalvikClassDef.forName(methPathClsName) match{
                   case Some(dccd) => {
                       dccd.lookupCurClsMethTable(methIndex) match {
                         case Some(md) => {List(md)}
                         case None => List()
                       }
                   }
                   case None => {List()}
                 }
                 
               }else{
                  List()
               }
            }
          }
        }
        case None => {
       
          List()
        }
      }
   }
   
   

  def lookupMethod(clsName: String, methPath: String, argTypes: List[String], invokeType: Int): List[MethodDef] = {
    	
    val methIndex = new CompactMethodIndex(methPath, argTypes)
    val MethClassDef = forName(clsName)
    
    //static or direct
    invokeType match{
      // static
      case 0 => {lookupStaticOrDirect(MethClassDef, methIndex)}
      // direct
      case 1 => {lookupStaticOrDirect(MethClassDef,methIndex)}
      //virtual 
      case 2 => {
         MethClassDef match {
         case Some(d) => {
          resolveMethod(d, methIndex)
        }
        case None => {
          Debug.prntDebugInfo("Virtual oR direct method lookup Failed! Class definition not found for method", methIndex)
          List()
        }
      }
      }
             // will just find the currnet objclass's super class and try find the method
         case 4 => {
            MethClassDef match {
         case Some(d) => {
           d.getSuperClsDefO match {
            case Some(sucl) => {
              sucl.lookupCurClsMethTable(methIndex) match{
                case Some(smd) => List(smd)
                case None => List()
              }
            }
            case None => {
              List()
            }
          }
        }
         }
         }
    }
}
}

case class InterfaceMethodSignature(methPath: String, sthTypeLst: List[String], retType: String)

case class MethodDef(methPath: String, ats: List[String], rn: BigInt, atl: List[String], retT: String, bd: Stmt, localHandlers: ExceptionHandlers, annotationExns: List[String]) {
  def regsNum = rn
  def attrs = ats
  val methodPath = methPath
  val argTypeList = atl
  val retType = retT
  val body = bd
  var isEntryPoint: Boolean = false
  
  override def toString = {
   val  stmts = CommonUtils.flattenLinkedStmt(List())(body) 
  /* val testSecondLin = stmts match 
  {
     case Nil => "empty next stmt"
     case hd :: tl => {
      hd.lineNumber
     }  
   }*/
   
    "\n" + "MethNamePath: " + methodPath + "\n" +  "RegNUm: " + rn.toString() + "\n" + "Modifiers: " + attrs + "\n" +
                             "Formal Types: " + argTypeList + "\n" +
                           "Return Type: " + retType + "\n" + "Stmts:" + stmts.length  //+ "\n" + "Second Stmt: " + testSecondLin
                           //body.toString() //CommonUtils.flattenLinkedStmt(List())(body) 
  } 
                          
}

// if we don't care the value for statically initialized values, then no this information for fieldDef objects
case class FieldDef(fp: String, atrs: List[String], ft: String) {
  def attrs = atrs
  def fieldPath = fp
  def fieldType = ft

}

class ParsingExpcetion(str: String) extends Exception


case class ExceptionHandler(typeStr: String, exnTy: String, fromLblStr: String, toLblStr: String, usingLblStr: String)  {

  override def toString =  "ExceptionHandler(" + typeStr + ","+ exnTy + "," + fromLblStr  + "," + toLblStr  +")"
}

case class ExceptionHandlers(handlerLst : List[ExceptionHandler]) {
  def getAllHandlerTypes : List[String] = handlerLst.map(_.exnTy)
  def getFinallyHandlers: List[ExceptionHandler] = handlerLst.filter((ty) => {
    if(ty == "finally") true else false
  })
  def getNonFinallyHandlers: List[ExceptionHandler] = handlerLst.filter((ty) =>  if(ty == "normal") true else false
  )
  
  def getNOnFinallyExnAndLabelPairs: List[(String, String)] = getNonFinallyHandlers.map((h) => (h.exnTy, h.usingLblStr))
  def getAllExnAndLabelPairs: List[(String, String)] = handlerLst.map((h) => (h.exnTy, h.usingLblStr))
  
  def getAllExnAndNxtStPairs: List[(String, Stmt)] = {
    val exnAndLblStrPairs = getAllExnAndLabelPairs.distinct
    
    exnAndLblStrPairs.foldLeft(List[(String, Stmt)]())((res, p) =>{
      val lblStr = p._2
      val stO = Stmt.forLabel(lblStr)
      stO match {
        case Some(st) => {
          val realN = CommonUtils.findNextStmtNotLineOrLabel(st)
         // if(realN.isInstanceOf[Stmt]) println("the next stmt for the label: " + lblStr + "is StmtNil. Source: getAllExnAndNxtStPairs")
          res ::: List((p._1, realN))
        }
        case None => {
          println("the label " + lblStr + "was not found in Stmt: Source, getAllExnAndNxtStPairs method")
          res 
        }
      }
    })
  }
  
   def getNonFinallyExnAndNxtStPairs: List[(String, Stmt)] = {
    val exnAndLblStrPairs = getNOnFinallyExnAndLabelPairs.distinct
    
    exnAndLblStrPairs.foldLeft(List[(String, Stmt)]())((res, p) =>{
      val lblStr = p._2
      val stO = Stmt.forLabel(lblStr)
      stO match {
        case Some(st) => {
          val realN = CommonUtils.findNextStmtNotLineOrLabel(st)
         // if(realN.isInstanceOf[Stmt]) println("the next stmt for the label: " + lblStr + "is StmtNil. Source: getAllExnAndNxtStPairs")
          res ::: List((p._1, realN))
        }
        case None => {
          println("the label " + lblStr + "was not found in Stmt: Source, getAllExnAndNxtStPairs method")
          res 
        }
      }
    })
  }
   
   
   def getMatchedHandlers(exnTypes: List[String]) : List[ExceptionHandler] = {
      handlerLst.filter((h) => {
        if(exnTypes.contains(h.exnTy)) true else false
      })
   }
}


// the Dalvik application 
sealed class Application {
  //  def classTable : ImmMap[SName, DalvikClassDef] 
  // more other information will be added
}







