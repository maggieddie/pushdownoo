package org.ucombinator.dalvik.specialAPIs
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.dalvik.cfa.cesk.CESKMachinary
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.syntax.{AExp, RegisterExp}
import org.ucombinator.utils.Debug
import org.ucombinator.dalvik.syntax.StForEqual


trait RawStringLibsAI extends StateSpace with CESKMachinary{

 import org.ucombinator.domains.CommonAbstractDomains._
  
  
   def isStringBulder(clsName: String) : Boolean = 
    if(clsName == "java/lang/StringBuilder") true else false
    
  /*private def isInvokingDirectStringBuilderInit(methPath: String) : Boolean = 
    if(methPath == "java/lang/StringBuilder/<init>") true else false
   
  private def isInvokeVirtualStringBuilderAppend(methPath: String): Boolean = {
      if(methPath == "java/lang/StringBuilder/append") true else false
    }
  private def isInvokeStaticStringValueof(methPath: String): Boolean = {
     if(methPath == "java/lang/String/valueOf") true else false
  }*/
    
  private def conservativeUpdateObjStore(s: Store, objVals: D, vals: D): Store = {
    objVals.toList.foldLeft(s)((resS, objValv) => {
      val objVal = objValv.asInstanceOf[ObjectValue]
      val objPointer = objVal.op
      val valueAddr = objPointer.offset("value")
      Debug.prntDebugInfo("the entry added to store for the init stringbuilder  object", (valueAddr, vals))
      storeUpdate(resS, List((valueAddr, vals)))
    })
  }
   
 
  /**
   * Append will make the "value" field in the object Pointer(s) all to top,
   * no matter what the other argument's absrtact values
   * and the return value will be bounded to all the the objVals. 
   * This sucks. the object pointers (values) for string will be many
   */
  def handleStringBuilderAppend(invokS: Stmt, argRegExps: List[AExp], objValss: D, ls: Stmt, s: Store, pst: PropertyStore, realN: Stmt, fp: FramePointer, kptr: KAddr, t: Time, tp: Time, k: Kont): Set[Conf] = {
  val objVals = filterStrBuilderObjVals(objValss, s)
   Debug.prntDebugInfo("the stringbuilder object values length is ", objVals.toList.length)
   val newStore = conservativeUpdateObjStore(s, objVals, s.mkDomainD(StringTop))
   val retAddr  = fp.offset("ret")
   // if we cast the object value to the parent type, what will happen?
   val newStore2 = storeUpdate(newStore, List((retAddr, s.mkDomainD(objVals.toList.map(_.asInstanceOf[Value]): _*))))
   Set(((PartialState(StForEqual(realN, realN.next, realN.clsPath, realN.methPath, realN.lineNumber), fp, newStore2, pst, kptr, tp), k)))
 }
 
 /**
  * This happens in invokeStatic stmt
  * there is one argument, which is refering to the string object pointer
  * we will get the "value" of the objPointer, and extend the store for the (ret,fp)
  */
  def handleStringValueof(invokS: Stmt, argRegExps: List[AExp], objVals: D, ls: Stmt, s: Store, pst: PropertyStore, realN: Stmt, fp: FramePointer, kptr: KAddr, t: Time, tp: Time, k: Kont): Set[Conf] = {
   val strobjvals = filterStrObjVals(objVals, s)
    Debug.prntDebugInfo(" filtered String object length " + strobjvals.toList.length , strobjvals)
    
    // this gonna join  the "value" from all the OP in the objVals
    val newValues = strobjvals.toList.foldLeft(s.mkDomainD())((resV, objValv) => {
      val objVal = objValv.asInstanceOf[ObjectValue]
      val strOp = objVal.op
      val valueAddr = strOp.offset("value")
      resV join storeLookup(s,valueAddr)
    })
    
    val newStore = storeUpdate(s, List((fp.offset("ret"), newValues)))
     Set(((PartialState(StForEqual(realN, realN.next, realN.clsPath, realN.methPath,realN.lineNumber), fp, newStore, pst, kptr, tp), k)))
 }
 
   def handleStringBuilderInit(invokS: Stmt, argRegExps: List[AExp], objValss: D //Set[ObjectValue]
   , ls: Stmt, s: Store, pst: PropertyStore, realN: Stmt, fp: FramePointer, kptr: KAddr, t: Time, tp: Time, k: Kont): Set[Conf] = {
    val objVals = filterStrBuilderObjVals(objValss, s)
    val strBuilderOp = ObjectPointer(t, "java/lang/StringBuilder", ls)
    val arglen = argRegExps.length
    arglen match {
      case 0 => {
        val newStore = conservativeUpdateObjStore(s, objVals, s.mkDomainD(StringLit("")))
        Set(((PartialState(StForEqual(realN, realN.next, realN.clsPath, realN.methPath,realN.lineNumber), fp, newStore, pst, kptr, tp), k)))
      }
      case 1 => {
        val argRegExp = argRegExps.head
        val regstr = argRegExp match {
          case re @ RegisterExp(_) => re.regStr
          case _ => { throw new CESKException("the argument of stringbuild init method is not register expression " + invokS) }
        }
        val argAddr = fp.offset(regstr)
        Debug.prntDebugInfo("the argument address of string builder init is: ", argAddr)
        val argVals = storeLookup(s, argAddr)
        Debug.prntDebugInfo(" the argvals of the stringinit arg", objVals.toList.length)

        // wow. this gonna make all the object Pointer mapped by the objAExp map the "value" to the argVal
        val newStore = conservativeUpdateObjStore(s, objVals, argVals)
        Set(((PartialState(StForEqual(realN, realN.next, realN.clsPath, realN.methPath, realN.lineNumber), fp, newStore, pst, kptr, tp), k)))
      }
      case _ => {
        //Set(((PartialState(realN, fp, s, kptr, tp), k)))
        throw new CESKException("StringBUilder init method has more than 1 arguments?!!! Deal with it! " + invokS) 
      }
    }
  }
   
 
}