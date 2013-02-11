/**
 *  1. Refactored the abstract domains definitions to CommonAbstractDomains
 *  	(I'm really sick of path dependent types)
 * 	2. Refactored the state definition to use top level Store. 
 * 
 * @author shuying
 */

package org.ucombinator.dalvik.cfa.cesk

import org.ucombinator.dalvik.syntax._
import scala.collection.immutable.{ Set => ImmSet, Map => ImmMap}
import org.ucombinator.utils.Debug
import org.ucombinator.utils.CommonUtils
import org.ucombinator.dalvik.informationflow.DalInformationFlow
import scala.util.matching.Regex
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.domains.CommonAbstractDomains
import org.ucombinator.domains.StandardDomains
  import CommonAbstractDomains._

trait StateSpace  {
  

  
  type :-> [T, S ] = ImmMap[T,S]
  // provided in particular impl
 // type Addr 
  // continuation addr
  type KAddr 
  
  type Kont
  
  // standard component for Dalvik
  //type Store = Addr :-> D //Set[Value]
  
  // mainly SecurityValue
  type PropertyStore = Store
  
  //conservative impl to inlcude time for states
  type Time 
  
  
  sealed trait Pointer { //extends Ordered[Pointer]{
    // def compare (that: Pointer) 
    // for any given pointer, we would like to converting offset into the 
    // address
    def offset (name: String) : Addr
  }
 // case class FramePointer(t: Time, meth: String) extends Pointer {
  case class FramePointer(t: Time, meth: Stmt) extends Pointer {
    def offset(regName: String) : RegAddr = new RegAddr(this, regName)
   
    /**
     * The push operation is used to generate another new Framepointer, 
     * with the current invokeStmt passed in, so that the other thing 
     * we can do at the same time is to set the live regs information to
     * the liveRegs, which will be used in GC
     */
    def push(t: Time, invkSt: Stmt // Stmt
        ): FramePointer = {
      
      new FramePointer(t, invkSt)
    } 
    override def toString = "FP(" + meth + " )"
    
    // it is said will not affect equality of the frame pointer
  //  var liveRegs : Set[String] = Set()

 
  }
  case class ObjectPointer(t: Time, clsName: String, lineNo: Stmt) extends Pointer {
    
    def offset(fieldName: String): FieldAddr = new FieldAddr(this, fieldName)
    def push(t: Time, ns: NewStmt, clsName: String, curFP: FramePointer): ObjectPointer = new ObjectPointer(t, clsName, lineNo)

  }
  
  /**********************************************
   * Various kinds of Addr in Dalvik analysis
   * 1.  RegAddr = FramePointer * RegName
   * 2.  FieldAddr = ObjectPointer * FieldName
   * 3.  KontAddr = not Known yet ...
   ********************************************/
  //sealed abstract  class Addr //extends some Ordering 
  
  abstract  class OffsetAddr extends Addr {
    def pointer : Pointer
    def offset : String
  
  }
  
  // kontinuation address.
  case class KontAddr(t: Time, st: Stmt) extends Addr {
    
  }
  
  
  case class RegAddr(fp: FramePointer, offs: String) extends OffsetAddr {
    def pointer = fp
    def offset = offs
    
    override def toString = "(" + fp.meth + "," + offset + ")"
  }
  
  case class FieldAddr(op: ObjectPointer, field: String) extends OffsetAddr {
    def pointer = op
    def offset = field
   
  }
  // ..
  
 
/************************
 * Frames in PDCFA
 ************************/
  abstract sealed class Frame
  
  case class HandleFrame(handlerType: String, exnType: String, l: String) extends Frame {
    def exceptionClasspath = exnType
    def handlerLabel = l
  }
  
  case class FNKFrame(st: Stmt, fp: FramePointer) extends Frame {
    def callerNextStmt = st
    def callerFP = fp
  }

  
  /**
   * for Object values
   */
   abstract class AbstractObjectValue extends Value
  case class ObjectValue(op: ObjectPointer, clsName: String) extends AbstractObjectValue {
    def oPointer: ObjectPointer = op
    def className = clsName
  }
  case class ObjectSomeTop(className: String)  extends AbstractObjectValue 
  
  
  
/**
 * Abstract values
 *//*
  sealed abstract class Value
  
  *//**
   * Security values, exclusively in the security store
   * a. clsP, methPath, lineNumber are context information 
   *    of the security object
   * b. secuOps: what operations
   * c. sourceOrSink: 0 for source, 1 for sink, 2 for both
   *//*
  // I think this is wrong. 
   // for the security values, the simple lattice there are only 8: 
  *//**
   * 1. gps
   * 2. sdcard
   * 3. filesystem
   * 4. picture
   * 5. time
   * 6. date
   * 7. phone
   * 8. sms
   * 
   * these form the flat lattice. of the abstract values.
   *//*
  case class SecurityValue(classPath: String,
					  methPath: String, 
					  lineNumber: Stmt,
					  secuOps: String,
					  sourceOrSink: Int) extends Value
  
  
  *//**
   * The rest defined values of other types
   *//*
  case class TrueValue() extends Value
  case class FalseValue() extends Value
  case object BoolTop extends Value
  case class NullValue() extends Value
  
  case class VoidValue() extends Value
  
  *//**
   * No sophisticated abstraction for int, float or double.
   * or we just use the top, don't care whether it is int,
   * long, or Bigint
   * 
   *//*
  abstract class AbstractNumLit extends Value
  case class NumLit(n: BigInt) extends AbstractNumLit
  case object NumTop extends AbstractNumLit
  def mkNumLit(n: BigInt): AbstractNumLit = {
    if (n > 2) {
      NumTop
    } else if (n < -2) {
      NumTop
    } else {
      NumLit(n)
    }
  }
  
  case class IntValue(val v: BigInt) extends Value {
    def value = v
  }
  
  *//**
   * FOr simplest string abstraction
   *//*
  abstract  class AbstractStringLiteral extends Value
  case class StringLit(str: String) extends AbstractStringLiteral
  case object StringTop extends AbstractStringLiteral
  
  *//**
   * for Object values
   *//*
   abstract class AbstractObjectValue extends Value
  case class ObjectValue(op: ObjectPointer, clsName: String) extends AbstractObjectValue {
    def oPointer: ObjectPointer = op
    def className = clsName
  }
  case class ObjectSomeTop(className: String)  extends AbstractObjectValue 
  
  // almost equal to the top of all values
  case object UnspecifiedVal extends Value
  
  // no context information aded into hte abstract vvalues
  case object Location extends Value
  case object FileSystem extends Value
  case object Sms extends Value
  case object Phone extends Value
  case object Picture extends Value
  case object DeviceID extends Value
  case object Network extends Value
  case object TimeOrDate extends Value
  case object SdCard extends Value
  case object ExecutableStr extends Value
  case object Display extends Value
  case object Voice extends Value
  case object ToBeDeTermined extends Value
  case object Reflection extends Value
  case object BrowserBookmark extends Value
  case object BrowserHistory extends Value
  case object DThread extends Value 
  case object IPC extends Value
  case object AMedia extends Value
  case object ASerialID extends Value
  case object AAccount extends Value
  case object ASensor extends Value
  case object AContact extends Value
  case object ARandom extends Value
  case object ADB  extends Value  */

  def genTaintKindValueFromStmt(stmt: Stmt, s: Store) :D = {
    val kindStrs = stmt.taintKind
    
    val res =
    kindStrs.foldLeft(Set[Value]()) ((res, kindStr) => { 
    kindStr match {
      case "sdcard" => res ++ Set(SdCard)
      case "filesystem" => res ++ Set(FileSystem)
      case "location" => res ++ Set(Location)
      case "phone" => res ++ Set(Phone)
      case "picture" => res ++ Set(Picture)
      case  "deviceid" => res ++ Set(DeviceID)
      case "network" => res ++ Set(Network)
      case "timeordate" => res ++ Set(TimeOrDate)
      case "sms" => res ++ Set(Sms) 
      case "executable" => res ++ Set(ExecutableStr)
      case "display" => res ++ Set(Display)
      case "voice"=> res ++ Set(Voice)
      case "tobedetermined"=> res ++ Set(ToBeDeTermined)
      case "reflection" => res ++ Set(Reflection)
      case "browserbookmark"=> res ++ Set(BrowserBookmark)
      case "browserhistory" => res ++ Set(BrowserHistory)
      case "thread" => res ++ Set(DThread)
      case "ipc" => res ++ Set(IPC)
      case "contact" => res ++ Set(AContact)
      case "sensor" => res ++ Set(ASensor)
      case "account" => res ++ Set(AAccount)
      case "media" => res ++ Set(AMedia)
      case "serialid" => res ++ Set(ASerialID)
      case "random" => res ++ Set(ARandom)
      case "database" => res ++ Set(ADB)
    }
     })
     s.mkDomainD(res.toList: _*)
  }

  /*********************
   * Framework related: 
   *********************/
  
  /**
   * if object something, then we record the clsType and return the objectop value
   * except for string, we return the StringTop
   */
  def typeToTopValue(typeStr: String, op:ObjectPointer, s: Store) : D = {
    // let's first ignore the array type????
     val arrayReg = """\(array \(object [^\s]+\)\)""".r //array of objects
     val arrayPrim = """\(array [^\s]+\)\)""".r // array of primitives
     
     val objP = """\(object [^\s]+\)""".r
    
     if (! objP.findAllIn(typeStr).toList.isEmpty) {
      
       val splitRes = typeStr.split("\\ ")
       val clsTypeP =  splitRes.toList(1)
       val clsType =if(clsTypeP.contains(")")) clsTypeP.substring(0,clsTypeP.length()-1) else clsTypeP
      
       clsType match {
        case "java/lang/String" => s.mkDomainD(StringTop)
        case _ => s.mkDomainD(ObjectValue(op,clsType))
      }
       
    } else {
      Debug.prntDebugInfo("not object or array", typeStr)
      typeStr match {
        case "int" | "float" | "double" | "long" | "short"  => s.mkDomainD(NumTop)
        case  "boolean" =>  s.mkDomainD(BoolTop)
        case "void" => s.mkDomainD(new VoidValue)
       // case "byte" | "char" => waht?
        case _ => s.mkDomainD( UnspecifiedVal)
      }
    }
  }
  
   
    // filter out or check to see whether there is any source or sink values recorded
   /* def srcOrSinksSecurityValues(vals: Set[Value]) : Set[Value] = {
     vals.filter((oneV) => {
       oneV match {
         case sv@SecurityValue(_, _, _, _, _) => {
           val srcOrSinkVal = sv.sourceOrSink
            srcOrSinkVal > 0
         case Location  |
         	  FileSystem | 
         	  Sms | 
         	  Phone | 
         	  Picture | 
         	  DeviceID | 
         	  Network | 
         	  TimeOrDate | 
         	  SdCard |
         	  Display |
         	  Voice |
         	  ToBeDeTermined |
         	  Reflection |
         	  BrowserBookmark |
         	  BrowserHistory | DThread => {
         	    true
         	  }
         case _ => false
       }
     })
   }*/
  
  /*private def pStoreHasTaintVals(pst: PropertyStore) : Boolean = {
    val allVals = pst.foldLeft(Set[Value]())((res: Set[Value], pair) => {
      val vals : Set[Value] = pair._2
      res ++ vals
    })
    val secuVals = srcOrSinksSecurityValues(allVals)
    secuVals.toList.length > 0
  }*/
  
  /*********************************************************************************************
   * Configurations are split into contorl states and continuation frames.
   * so that PDA and kCFA can coexisit in one framework
   *********************************************************************************************/
   
  abstract sealed class ControlState {
    
    // compare this state with another state based on partial orderness
    
    def isPartialState : Boolean = {
     this match {
      case ErrorState(_, _,_) | FinalState(_) => false
      case PartialState(st, fp, store, ps, kptr,  t) => {
         true
      }
    }
    }
    
   def isPopHandlerState : Boolean = {
    	 val curStO = this.getCurSt
       curStO match {
         case Some(st) => {
           val oldStyleSt = st.oldStyleSt
           oldStyleSt match {
             case ps@PopHandlerStmt(exnType , nxt , ls , clsP , methP) => {
               //println("got an pop handler control state", ps)
               true
             }
             case _ => false
           }
         }
         case None => false
       }
   }
   
   
    def weakerThan(that: ControlState, subsumption: Boolean) : Boolean = {
      if(this.isPartialState && that.isPartialState) {
        this match {
          case PartialState(stmt, fp, s, pst, kptr, t) => {
            that match{
              case PartialState(stmti, fpt, st, pstt, kptrt, tt) => {
               
                val cond1 =  (stmt.equals(stmti) )     
                val cond2 = (fp.equals( fpt))  
                var cond3 = false
                var cond4 = false
                if(subsumption) {
                    cond3 =   s.isSubsumedBy(st)   //partialOrderStoreCompare(s, st)   
                 cond4 =   pst.isSubsumedBy(pstt)//partialOrderStoreCompare(pst, pstt)  
                }
                else{
                  cond3 = s.equal(st)
                  cond4 = pst.equal(pstt)
                }
               
                val cond5 =   (kptr.equals(kptrt) ) 
                val cond6 = (t.equals( tt))
                //println(stmt)
                //println(stmti)
               // println(cond1.toString + " " + cond2.toString + " " + cond3.toString + " " + cond4.toString + " " + cond5.toString + " " + cond6.toString)
                 cond1 && cond2 && cond3 && cond4 && cond5 && cond6
              }
              case _ => false //not possible
            }
          }
          case _ => {
            false // not possible
          }
          }
        } 
      else false
      } 
    
    // helper function that decide if a control is weaker than a set of (seen) states
    def weakerThanAny(  seenStates: Set[ControlState], subsumption: Boolean) : Boolean = {
      val res = seenStates.filter(ss => {
        this.weakerThan(ss, subsumption)
      })
      ! res.isEmpty
    }
    
    
    def getStmtForEqual: Option[StForEqual] = {
      this match {
      case ErrorState(_, _,_) | FinalState(_) => None
      case PartialState(st, fp, store, ps, kptr,  t) => {
        Some(st)
      }
    }
    }
      
    def sourceOrSinkState : Boolean = {
      this match{
        case ps@PartialState(st, fp, s, pst, kptr, t) => {
       
          val sourceOrSink = ps.st.oldStyleSt.sourceOrSink 
          sourceOrSink > 0 // || pStoreHasTaintVals(pst)
        }
        case _ => false
      } 
    }
    
    def taintKind: Set[String] ={
       this match{
        case ps@PartialState(st, fp, s, pst, kptr, t) => {
       
            ps.st.oldStyleSt.taintKind
            // || pStoreHasTaintVals(pst)
        }
        case _ => Set("")
      } 
    }
    
    def getRiskRanking: Int = { 
         this match{
        case ps@PartialState(st, fp, s, pst, kptr, t) => {
        	st.oldStyleSt.riskRanking
        }
        case _ =>  0
      } 
    }
    
    // tempararily class name + method name and line number
    def getSourceLocation : (String, String, String) = {
       this match{
        case ps@PartialState(st, fp, s, pst, kptr, t) => {
        	(st.clsPath, st.methPath, st.lineSt.asInstanceOf[LineStmt].linenumber)
        }
        case _ =>  ("", "", "")
      } 
    }
    
    
      def matchRegex(regexR: Regex) : Boolean = {
      this match {
       case ps@PartialState(st, fp, s, pst, kptr, t) => { 
    	    
    	    // matching the statement string
    	    // not precise
    	    val matchArea = ps.st.oldStyleSt.toString.split("@@@").toList
    	    if(regexR == null) false
    	    else{
    	      if(matchArea.isEmpty) false
    	      else{
    	    	regexR.pattern.matcher(matchArea(0)).matches()
    	      }
    	    }
            // || pStoreHasTaintVals(pst)
        }
        case _ =>  false 
    }
      }
    
    def taintedState : Boolean = {
      this match{
        case ps@PartialState(st, fp, s, pst, kptr, t) => {  
             pst.pStoreHasTaintVals//(pst)
        }
        case _ => false
      } 
    }
    
    def getCurSt: Option[StForEqual] = {
     this match{
        case ps@PartialState(st, fp, s, pst, kptr, t) => {  
            Some(st)
        }
        case _ =>  None
      } 
    }
    
    
    def getCurStore: Store = {
     this match{
        case ps@PartialState(st, fp, s, pst, kptr, t) => {  
            s
        }
        case FinalState(st) => st.mkEmptyStore
        case ErrorState(_, _, st) => st.mkEmptyStore
      }
    }
    
    
     def getCurPropertyStore: PropertyStore= {
     this match{
        case ps@PartialState(st, fp, s, pst, kptr, t) => {  
            pst
        }
        case FinalState(st) => st.mkEmptyStore
        case ErrorState(_, _, st) => st.mkEmptyStore
      }     
      
     }
    
    //for widening
    def getCurFreq  : Int =  {
    
    val stqO  = this.getStmtForEqual
    stqO match {
      case None => 0
      case Some(stq) => Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningCounterTbl(stq)
    } 
  }

    def getCurWidenedStore: (Store, Store) = {
      val stqO = this.getStmtForEqual

      stqO match {
        case None => {
          this match {
            case ps @ PartialState(st, fp, s, pst, kptr, t) => {
              (s.mkEmptyStore, pst.mkEmptyStore)
            }
            case FinalState(st) => (st.mkEmptyStore, st.mkEmptyStore)
            case ErrorState(_, _, st) => (st.mkEmptyStore, st.mkEmptyStore)
          }
        }  
        case Some(stq) => {
          val tbl = Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningStoreTbl
          if (tbl.contains(stq)) {
            //Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningStoreTbl.foreach(println)
            val (s, ps) = Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningStoreTbl(stq)
            (s.asInstanceOf[Store], ps.asInstanceOf[Store])
          } else {
            this match {
              case ps @ PartialState(st, fp, s, pst, kptr, t) => {
                (s.mkEmptyStore, pst.mkEmptyStore)
              }
              case FinalState(st) => (st.mkEmptyStore, st.mkEmptyStore)
              case ErrorState(_, _, st) => (st.mkEmptyStore, st.mkEmptyStore)
            }
          }
        }
      }
    }
    
    // side effect the table
    def updateWideningFreqTbl {
       val stqO  = this.getStmtForEqual
    stqO match {
      case None => 0
      case Some(stq) => {
        val gblFreQTbl = Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningCounterTbl 
        if(gblFreQTbl.contains(stq))
         Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningCounterTbl  +=  (stq -> (gblFreQTbl(stq)+1) )
        else
          Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningCounterTbl  +=  (stq -> 0 )
      }
    } 
    }
 
    
    def updateWideningStoreTbl(s: Store, ps: PropertyStore) {
       val stqO  = this.getStmtForEqual
    stqO match {
      case None => 0 //doing nothing
      case Some(stq) => {
        val gblStoreTbl = Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningStoreTbl 
        if(gblStoreTbl.contains(stq))
         Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningStoreTbl  +=  
           (stq -> (s,ps))   
        else {
         // Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningStoreTbl  +=  
          // (stq -> (s,ps))
        }
          //Thread.currentThread().asInstanceOf[AnalysisHelperThread].ppwWideningStoreTbl  +=  (stq -> 0 )
      }
    } 
    }
  
     var intentRelatedInfo: Map[(String, String, Stmt), Map[String, Set[IntentExtraKeyTypeAndValue]]] = Map.empty
  }
 
  
 
 // case class PartialState(st: Stmt, fp: FramePointer, s: Store, kptr: KAddr, t:Time) extends ControlState
  case class PartialState(st: StForEqual, fp: FramePointer, s: Store, ps: Store, kptr: KAddr, t:Time) extends ControlState {
  
    
    override  def toString ={
      
      "Statement: " + st.oldStyleSt + "\n" + "fp: "+ fp.toString + "\n" +  "store: " + s
    } 
  }
  
  case class FinalState(s: Store) extends ControlState
  case class ErrorState(s: Stmt, msg: String, st: Store) extends ControlState
 

  
  type Conf = (ControlState, Kont)
  
  /**
   * Injection an stmt into a program
   * @param s initial stmt
   * */
  
  def initState(s: Stmt, methP: String, store:Store, pStore: PropertyStore): Conf
  
  /***
   *  Address and Allocations
   * 
   */
  // abstract member
  def k : Int
   

  /******************************************************
   * Utility functions
   ******************************************************/
// store compare
  // s1 <= s2? == s1.issubsumedBy
 /* def partialOrderStoreCompare(s1: Store, s2: Store) : Boolean = {
    val res = s1.map(kv => {
      if(s2.contains(kv._1)){
        val absV2 = s2.get(kv._1)
         absV2 match {
          case Some(vs) => {
             kv._2 subsetOf vs // should call a function to compare the set of the orderness.
          }
          case None => {
            false // not possible
          }
        }
      }
      else false
    })
    
    val l = s1.size
    val resL = res.size
    l == resL
  }*/
  
 
  
  def storeLookup(s: Store, a: Addr) : D = //Set[Value] =
  {  
    //import org.ucombinator.domains.GodelDomains._
    
//    s.get(a) match {
//    case Some(x) => {
//      Debug.prntDebugInfo("Found value " , x)
//      x
//    }
//    case None => {
//      Debug.prntDebugInfo("Empty set ", Set() )
//      Set()
//    }
    //case None => throw new SemanticException("StoreLookUp Exception: No values found for address" + a.toString())
     s.getOrElse(a)
  }
   
  
  
  def storeUpdate(s: Store, pairs: List[(Addr, D)]) = //Set[Value])]) = 
   s ++ pairs
      /*pairs.foldLeft(s)((accum, pair) => {
      val (a, vs) = pair
      val oldVals: D = accum.getOrElse(a)
      val newVals: D = oldVals join vs.filterNot(UnspecifiedVal)
      Debug.prntDebugInfo("storeUpdate entry: ", (a, newVals))
      accum + (a, newVals)
    })*/
    
    // tmp use of the strong updates
   // this shoudl be deleted !!!!!!
   def storeStrongUpdate(s: Store, pairs: List[(Addr, D)]) ={ //Set[Value])]) = {
     pairs.foldLeft(s)((accum, pair) => {
      val (a, vs) = pair
      val oldVals: D = accum.getOrElse(a)
      // we dont care the original values in the store
      val newVals: D =  vs.filterNot(UnspecifiedVal)
      Debug.prntDebugInfo("storeUpdate entry: ", (a, newVals))
      accum + (a, newVals)
    })
  }
  
  /******
   * OK... just for the sake of concept clarity, here is duplicated code for Property store
   * use storeLookup!!
   *//*
   def pStoreLookup(s: PropertyStore, a: Addr) : Set[Value] =
  {  
    s.get(a) match {
    case Some(x) => { 
     //  println("PStore: NonEmoty Store"+ x)
      x
    }
    case None => {
    //  println("PStore: Empty set ", Set() )
      Set()
    }
    //case None => throw new SemanticException("StoreLookUp Exception: No values found for address" + a.toString())
  }
  }
  
  
  def pStoreUpdate(s: PropertyStore, pairs: List[(Addr, Set[Value])]) = {
     
      pairs.foldLeft(s)((accum, pair) => {
      val (a, vs) = pair
      val oldVals: Set[Value] = accum.getOrElse(a, Set())
      val newVals: Set[Value] = oldVals ++ vs.filter(v => v != UnspecifiedVal)
      Debug.prntDebugInfo("storeUpdate entry: ", (a, newVals))
      accum + ((a, newVals))
    })
  }
    
    // tmp use of the strong updates
   def pStoreStrongUpdate(s: PropertyStore, pairs: List[(Addr, Set[Value])]) = {
     pairs.foldLeft(s)((accum, pair) => {
      val (a, vs) = pair
      val oldVals: Set[Value] = accum.getOrElse(a, Set())
      // we dont care the original values in the store
      val newVals: Set[Value] =  vs.filter(v => v != UnspecifiedVal)
      Debug.prntDebugInfo("storeUpdate entry: ", (a, newVals))
      accum + ((a, newVals))
    })
  }*/
      
  /*******/
      
   def isMove(opCode: SName) : Boolean = {
      import CommonSSymbols._;
       opCode match {
         case  SMove | SMove16| SMoveWide | SMoveWide16| SMoveWideFrom16 | SMoveObject 
         |  SMoveObjectFrom16 | SMoveObject16 | SMoveFrom16 => true
         case _ => false
       }
   }
 
  
   
   def getRegExp(ae: AExp, errStr : String) : RegisterExp = {
     ae match {
         case RegisterExp(sv) => {
           ae.asInstanceOf[RegisterExp]
         }
         case _ => {
           throw new SemanticException(errStr + ae.toString())
         }
     }
   }
   
    /**
   * Store merging machinery
   * For single-store passing optimization
   * as well as for computing statistics
   * 
   * s1 is in the result, now you have got the s2's entry into the result
   * 
   */
/*  def mergeTwoStores[K, V](s1: K :-> Set[V], s2: K :-> Set[V]): K :-> Set[V] = {
    s2.foldLeft(s1)((resultStore: K :-> Set[V], keyValue: (K, Set[V])) => {
      // these are from s2
      val (k, vs) = keyValue
      // these are from s1
      val newValues = s1.getOrElse(k, Set())
      resultStore + ((k, vs ++ newValues))
    })
  }*/
   
     def mergeTwoStores (s1: Store, s2: Store): Store = {
       s1.join(s2)
   /* s2.foldLeft(s1)((resultStore: K :-> Set[V], keyValue: (K, Set[V])) => {
      // these are from s2
      val (k, vs) = keyValue
      // these are from s1
      val newValues = s1.getOrElse(k, Set())
      resultStore + ((k, vs ++ newValues))
    })*/
  }
   
  // wil merge all the stores
/* def mergeStores[K, V](
     initial: K :-> Set[V], 
     newStores: List[K :-> Set[V]]): K :-> Set[V] = {
    newStores.foldLeft(initial)((result, current) =>
    mergeTwoStores(result, current))
  }*/
     
      def mergeStores (
     initial:Store, 
     newStores: List[Store]): Store = {
    newStores.foldLeft(initial)((result, current) =>
    mergeTwoStores(result, current))
  }
  
  def getMonovariantStore(states: Set[ControlState], store: Store): Store = {
    
     
     val allRegularStores =  states.map {
      case PartialState(_, _, s, _,_,_) => s
      case FinalState(_) => store.mkEmptyStore
     }
     val  sss : List[Store] =  allRegularStores.toList
    mergeStores(store.mkEmptyStore, sss)
  }
  
 /*  def getMonovariantPStore(states: Set[ControlState]): Addr:-> Set[Value] = {
    
     val allRegularStores =  states.map {
      case PartialState(_, _, _, pst,_,_) => pst
     }
     val emptyMonovariantStore : Addr :-> Set[Value] = ImmMap.empty
    mergeStores(emptyMonovariantStore, allRegularStores.toList)
  }*/
  
      def filterRegisterStates (states: Set[ControlState]): Set[ControlState] = {
     states.filter({
      case PartialState(_, _, _, _,_,_) => true
      case _ => false
    })
   }
      
   def wideningState(oldS: ControlState, newStore: Store, newPropertyStore: PropertyStore): ControlState = {
     oldS match{ 
       case PartialState(st , fp , s , ps , kptr , t ) => PartialState(st , fp , newStore , newPropertyStore , kptr , t )
       case _ => oldS
     }
   }
   
   // give a set of new states, filter out the ones that weaker than the seen set (which will be passed)
   def getWeakerStates (possibleStates: Set[ControlState], seenStates: Set[ControlState], subsumption: Boolean) : Set[ControlState] = {
     possibleStates.filter(ps => {
        ps.weakerThanAny(seenStates, subsumption)
     })
   }
    
   
     
  
   

  class SemanticException(s: String) extends Exception(s)


}

