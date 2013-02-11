package org.ucombinator.dalvik.cfa.cesk
import org.ucombinator.dalvik.syntax._
import org.ucombinator.utils.StringUtils
import org.ucombinator.dalvik.vmrelated.DalvikVMRelated
import org.ucombinator.utils.Debug


/** 
 *  Basic functionality of CESK-based formalisms
 *  Two kinds of subtypes: 
 *  for convinient, stil the time-stamped CESK
 *  
 *  1. Frame-based CESK for PDCFA
 *  2. Pointer-based CESK for traditional kCFA
 */

trait CESKMachinary extends StateSpace with DalvikVMRelated {

 /*****************
  * Utility Functions 
  *****************/

  
  def atomEval(ae: AExp, fp: FramePointer, s:Store) : Set[Value] = {
    ae match {
      case BooleanExp(s) => {
        s.toString() match {
          case "#t" => Set( BoolTop )
          case "#f" => Set( BoolTop)
        }
      }
      case IntExp(si) => {
        //here we go: return the number as top
        Set(NumTop)
       // Set( IntValue(si.value))
      }
      case n: NullExp => Set(new NullValue)
      case ve: VoidExp => Set(new VoidValue)
      
      case re@(RegisterExp(sv)) => {
       val regAddr =  fp.offset(re.regStr)
       storeLookup(s,regAddr)
      }
      
      // For string exp evaluation
      case sle@StringLitExp(_) => {
       Set(StringLit(sle.strLit))
      }
      //case FieldExp(fieldPath, fieldType) => {
        
     // }
     // case (regExp: RegisterExp(sv)) =>{
        
    //  }
      
    }
  }
  
  def getReturnOffSet(fp: FramePointer) : Addr = {
    fp.offset("ret")
  }

  /**
   * don't need
   */
   def isAtomic(ae: AExp): Boolean = ae match {
    case BooleanExp(_) => true
    case IntExp(_) => true
    case n: NullExp => true
    case ve: VoidExp => true
    case re@(RegisterExp(sv))  => true
    case _ => false
  }
   
  /**
   * sucks to have to detect this. could be done at the front end for each case
   * yeah, for safety, we'd better to check
   */
   
   def isConstNum(opCode:SName) : Boolean = {
       import CommonSSymbols._;
     opCode match {
       case  SConst4 | SConst16 | SConst  | SConstHigh16 |  SConstWide16 |   SConstWide32 |  SConstWide 
    | SConstWideHigh16 //| SConstString |SConstStringJumbo | SConstClass  
  
     => true
     
       case _ => false
     }
   }
   def isPrimitiveNumerical(opCode: SName) : Boolean = {
        import CommonSSymbols._;
     opCode match {
       case  SNegInt  | 
       SNotInt | SNegLong  |SNotLong | SNegFloat |
     SNegDouble| SIntToLong | SIntToFloat| SIntToDouble | SLongToInt | SLongToFloat | SLongToDouble|
     SFloatToInt |SFloatToLong |   SFloatToDouble |   SDoubleToInt |   SDoubleToLong | SDoubleToDouble |
     SIntToByte |     SIntToChar |     SIntToShort  | 
     
     SAddInt |     SSubInt |    SMulInt | SDivInt|SRemInt | SAndInt |  SOrInt | SXorInt|  SShlInt | 
     SShrInt |   SUShrInt |SAddLong | SSubLong | SMulLong| SDivLong |   SRemLong|  SAndLong |  SOrLong | 
     SXorLong |  SShlLong |  SShrLong |SUShrLong | SAddFloat | SSubFloat | SMulFloat |SDivFloat | SRemFloat | 
     SAddDouble |  SSubDouble|  SMulDouble | SDivDouble |    SRemDouble| 
     
     SAddInt2Addr |     SSubInt2Addr | SMulInt2Addr |SDivInt2Addr| SRemInt2Addr |SAndInt2Addr | SOrInt2Addr|
     SXorInt2Addr|SShlInt2Addr|SShrInt2Addr |SUShrInt2Addr|SAddLong2Addr |SSubLong2Addr |SMulLong2Addr |
     SDivLong2Addr | SRemLong2Addr | SAndLong2Addr | SOrLong2Addr | SXorLong2Addr|   SShlLong2Addr | SShrLong2Addr|
     SUShrLong2Addr |SAddFloat2Addr |SSubFloat2Addr |SMulFloat2Addr | SDivFloat2Addr |SRemFloat2Addr |SAddDouble2Addr |
     SSubDouble2Addr | SMulDouble2Addr |SDivDouble2Addr|SRemDouble2Addr |
     
     SAddIntLit8 |  SRSubIntLit8 |SDivIntLit8 | SRemIntLit8 |  SAndIntLit8 | SOrIntLit8 |SXorIntLit8 | SShlIntLit8 |
     SShrIntLit8 | SUShrIntLit8 | SAddIntLit16 | SRSubIntLit16 | SMulIntLit16 | SDivIntLit16 | SRemIntLit16 
     | SAndIntLit16 | SOrIntLit16 | SXorIntLit16 |
     
     SConst4 | SConst16 | SConst  | SConstHigh16 |  SConstWide16 |   SConstWide32 |  SConstWide 
    | SConstWideHigh16  | //| SConstString |SConstStringJumbo | SConstClass  |
  
    SCmplFloat |  SCmpgFloat |  SCmplDouble | SCmpgDouble| SCompLong  
     => true
     
       case _ => false
     }
   }

  def isConstString(opCode: SName): Boolean = {
    import CommonSSymbols._
    opCode match {
      case SConstString | SConstStringJumbo => true
      case _ => false
    }
  }
  
  /**
   * special instruction of const-class
   */
  def isConstClass(opCode: SName) : Boolean = {
    import CommonSSymbols._
    opCode match {
      case  SConstClass => true
      case _ => false
    }
  }
  
  def isCheckCast(opCode: SName): Boolean = {
     import CommonSSymbols._
    opCode match {
      case  SCheckCast => true
      case _ => false
    }
    
  }
   
  def filterAbsObjValues(objs: Set[Value]) : Set[AbstractObjectValue] = {
     val vs = 
      objs filter ((o: Value) => o match {
      case ob@(ObjectValue(_,_)) => true
      case ObjectSomeTop(_) => true // should we need this at all? yes. we need to pass the topObject around.
      case _ => false
    })
    vs.map(_.asInstanceOf[AbstractObjectValue])

  }
  
  def filterObjValues(objs: Set[Value]) : Set[ObjectValue] = {
    val vs = 
      objs filter ((o: Value) => o match {
      case ob@(ObjectValue(_,_)) => true
     // case ObjectSomeTop(_) => true // should we need this at all? yes. we need to pass the topObject around.
      case _ => false
    })
    vs.map(_.asInstanceOf[ObjectValue])
   /*vs.map ((s)=>{
     s match {
       case ob@(ObjectValue(_,_)) => {s.asInstanceOf[ObjectValue]}
       case ObjectSomeTop(_) => {s.asInstanceOf[ObjectSomeTop]}
     }
   })*/
  }
  
  def filterStrObjVals(objVals: Set[ObjectValue]) : Set[ObjectValue] ={
    objVals.filter(_.className == "java/lang/String")
  }
  
  def filterStrBuilderObjVals(objVals: Set[ObjectValue]) : Set[ObjectValue] ={
    objVals.filter(_.className == "java/lang/StringBuilder")
  }
  
  def filterClassObjVals(objVals: Set[ObjectValue]) : Set[ObjectValue] ={
     objVals.filter(_.className == "java/lang/Class")
  }
  
  // used in instance method call: entry point invoking
  // not very safe operation here!
  def getObjVal(vals: List[Set[Value]]): AbstractObjectValue = {
    val firstVal = vals.head.toList.head
    firstVal match {
      case ob@(ObjectValue(_,_)) => ob
      case obo@ObjectSomeTop(_) => obo //
      case _ => throw new CESKException("the first val of entry point is not ObjectValue")
    }
  }
  
  /**
   * called in new instance transition
   * class instantication: have the current class type memory map
   * as well as all the super class memory map
   */
  
   def getFieldTypeStrs (clsName: String) : List[(String, String)] = {
    val clsDefO = DalvikClassDef.forName(clsName)
   
    clsDefO match {
      case Some(cd) => {
        val fields = cd.getAllFields
         Debug.prntDebugInfo("fields of clsName: " + clsName, fields)
      fields
      }
      case None => {
        List()
      }
    }
  }
   /**
    * modifed to do strong udpate  along with the allocation site NewStmt
    */
  def initObject(classPath: String, s: Store, op: ObjectPointer) : Store ={
    val fieldPathStrs = getFieldTypeStrs(classPath)
    val fieldOffsets = fieldPathStrs.map {
      case (path, ftype) => op.offset(path)
    }
    
    val ps = fieldOffsets map ((_, Set[Value]()))
    //storeUpdate(s, ps) strong update
    storeStrongUpdate(s,ps)
  }
  
 
   def canHaveEmptyContinuation(c: ControlState) = c match {
    case FinalState() => true
    case ErrorState(_, _) => true
    /**
     * Be careful with the following!
     * when can the parital state have empty continuation?
     * Currnently NO?
     */
    case PartialState(st, fp, store, kptr, t) => true
    //  if isAtomic(ae) => true
    case _ => false
  }

  def mustHaveOnlyEmptyContinuation(c: ControlState) = c match {
    case FinalState() => true
    case _ => false
  }

  class CESKException(s: String) extends SemanticException(s)
}