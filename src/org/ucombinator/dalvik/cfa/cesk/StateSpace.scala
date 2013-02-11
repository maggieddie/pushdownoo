package org.ucombinator.dalvik.cfa.cesk

import org.ucombinator.dalvik.syntax._
import scala.collection.immutable._
import org.ucombinator.utils.Debug
import org.ucombinator.utils.CommonUtils

trait StateSpace {
  
  type :-> [T, S ] = Map[T,S]
  // provided in particular impl
 // type Addr 
  // continuation addr
  type KAddr 
  
  type Kont
  
  // standard component for Dalvik
  type Store = Addr :-> Set[Value]
  
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
  sealed abstract case class Addr //extends some Ordering 
  
  abstract case class OffsetAddr extends Addr {
    def pointer : Pointer
    def offset : String
  
  }
  
  case class KontAddr extends Addr
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
 * Abstract values
 */
  sealed abstract class Value
  case class TrueValue extends Value
  case class FalseValue extends Value
  case object BoolTop extends Value
  case class NullValue extends Value
  
  case class VoidValue extends Value
  
  /**
   * No sophisticated abstraction for int, float or double.
   * or we just use the top, don't care whether it is int,
   * long, or Bigint
   * 
   */
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
  
  /**
   * FOr simplest string abstraction
   */
  abstract case class AbstractStringLiteral extends Value
  case class StringLit(str: String) extends AbstractStringLiteral
  case object StringTop extends AbstractStringLiteral
  
  /**
   * for Object values
   */
   abstract class AbstractObjectValue extends Value
  case class ObjectValue(op: ObjectPointer, clsName: String) extends AbstractObjectValue {
    def oPointer: ObjectPointer = op
    def className = clsName
  }
  case class ObjectSomeTop(className: String)  extends AbstractObjectValue 
  
  // almost equal to the top of all values
  case object UnspecifiedVal extends Value
  

  /*********************
   * Framework related: 
   *********************/
  
  /**
   * if object something, then we record the clsType and return the objectop value
   * except for string, we return the StringTop
   */
  def typeToTopValue(typeStr: String, op:ObjectPointer) : Set[Value] = {
    // let's first ignore the array type????
     val arrayReg = """\(array \(object [^\s]+\)\)""".r //array of objects
     val arrayPrim = """\(array [^\s]+\)\)""".r // array of primitives
     
     val objP = """\(object [^\s]+\)""".r
    
     if (! objP.findAllIn(typeStr).toList.isEmpty) {
      
       val splitRes = typeStr.split("\\ ")
       val clsTypeP =  splitRes.toList(1)
       val clsType =if(clsTypeP.contains(")")) clsTypeP.substring(0,clsTypeP.length()-1) else clsTypeP
      
       clsType match {
        case "java/lang/String" => Set(StringTop)
        case _ => Set(ObjectValue(op,clsType))
      }
       
    } else {
      Debug.prntDebugInfo("not object or array", typeStr)
      typeStr match {
        case "int" | "float" | "double" | "long" | "short"  => Set(NumTop)
        case  "boolean" =>  Set(BoolTop)
        case "void" => Set(new VoidValue)
       // case "byte" | "char" => waht?
        case _ => Set( UnspecifiedVal)
      }
    }
  }
  
  /*********************************************************************************************
   * Configurations are split into contorl states and continuation frames.
   * so that PDA and kCFA can coexisit in one framework
   *********************************************************************************************/
   
  abstract sealed class ControlState
  
  /**
   * Life the old not rich st to the follwoing
   */

  
 // case class PartialState(st: Stmt, fp: FramePointer, s: Store, kptr: KAddr, t:Time) extends ControlState
  case class PartialState(st: StForEqual, fp: FramePointer, s: Store, kptr: KAddr, t:Time) extends ControlState
  
  case class FinalState() extends ControlState
  case class ErrorState(s: Stmt, msg: String) extends ControlState
 

  
  type Conf = (ControlState, Kont)
  
  /**
   * Injection an stmt into a program
   * @param s initial stmt
   * */
  
  def initState(s: Stmt, methP: String): Conf
  
  /***
   *  Address and Allocations
   * 
   */
  // abstract member
  def k : Int
   
 //def alloc() : Addr

 
  

  
  /******************************************************
   * Utility functions
   ******************************************************/

  def storeLookup(s: Store, a: Addr) : Set[Value] =
  { Debug.prntDebugInfo("storeLookup: address: " , a)
    s.get(a) match {
    case Some(x) => {
      Debug.prntDebugInfo("Found value " , x)
      x
    }
    case None => {
      Debug.prntDebugInfo("Empty set ", Set() )
      Set()
    }
    //case None => throw new SemanticException("StoreLookUp Exception: No values found for address" + a.toString())
  }
  }
  
  
  def storeUpdate(s: Store, pairs: List[(Addr, Set[Value])]) = 
   
      pairs.foldLeft(s)((accum, pair) => {
      val (a, vs) = pair
      val oldVals: Set[Value] = accum.getOrElse(a, Set())
      val newVals: Set[Value] = oldVals ++ vs.filter(v => v != UnspecifiedVal)
      Debug.prntDebugInfo("storeUpdate entry: ", (a, newVals))
      accum + ((a, newVals))
    })
    
    // tmp use of the strong updates
   def storeStrongUpdate(s: Store, pairs: List[(Addr, Set[Value])]) = {
     pairs.foldLeft(s)((accum, pair) => {
      val (a, vs) = pair
      val oldVals: Set[Value] = accum.getOrElse(a, Set())
      // we dont care the original values in the store
      val newVals: Set[Value] =  vs.filter(v => v != UnspecifiedVal)
      Debug.prntDebugInfo("storeUpdate entry: ", (a, newVals))
      accum + ((a, newVals))
    })
  }
      
      
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
  def mergeTwoStores[K, V](s1: K :-> Set[V], s2: K :-> Set[V]): K :-> Set[V] = {
    s2.foldLeft(s1)((resultStore: K :-> Set[V], keyValue: (K, Set[V])) => {
      // these are from s2
      val (k, vs) = keyValue
      // these are from s1
      val newValues = s1.getOrElse(k, Set())
      resultStore + ((k, vs ++ newValues))
    })
  }
   
  // wil merge all the stores
 def mergeStores[K, V](
     initial: K :-> Set[V], 
     newStores: List[K :-> Set[V]]): K :-> Set[V] = {
    newStores.foldLeft(initial)((result, current) =>
    mergeTwoStores(result, current))
  }
  
  def getMonovariantStore(states: Set[ControlState]): Addr:-> Set[Value] = {
    
     val allRegularStores =  states.map {
      case PartialState(_, _, s, _,_) => s
     }
     val emptyMonovariantStore : Addr :-> Set[Value] = Map.empty
    mergeStores(emptyMonovariantStore, allRegularStores.toList)
  }
  
      def filterRegisterStates (states: Set[ControlState]): Set[ControlState] = {
     states.filter({
      case PartialState(_, _, _, _,_) => true
      case _ => false
    })
   }

  class SemanticException(s: String) extends Exception(s)


}

