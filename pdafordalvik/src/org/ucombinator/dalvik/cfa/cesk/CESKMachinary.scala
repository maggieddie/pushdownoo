package org.ucombinator.dalvik.cfa.cesk
import org.ucombinator.dalvik.syntax._
import org.ucombinator.utils.StringUtils
import org.ucombinator.dalvik.vmrelated.DalvikVMRelated
import org.ucombinator.utils.Debug
import org.ucombinator.dalvik.informationflow.DalInformationFlow
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.utils.CommonUtils.HeatPair


/** 
 *  Basic functionality of CESK-based formalisms
 *  Two kinds of subtypes: 
 *  for convinient, stil the time-stamped CESK
 *  
 *  1. Frame-based CESK for PDCFA
 *  2. Pointer-based CESK for traditional kCFA
 */

trait CESKMachinary extends StateSpace {// with DalvikVMRelated {

  import org.ucombinator.domains.CommonAbstractDomains._
 /*****************
  * Utility Functions 
  *****************/

  
  def atomEval(ae: AExp, fp: FramePointer, st: Store) : D = {// Set[Value] = {
    ae match {
      case BooleanExp(s) => {
        s.toString() match {
          case "#t" => st.mkDomainD(BoolTop)  //Set( BoolTop )
          case "#f" => st.mkDomainD(BoolTop)// Set( BoolTop)
        }
      }
      case IntExp(si) => {
        //here we go: return the number as top
        st.mkDomainD(NumTop)
       // Set( IntValue(si.value))
      }
      case n: NullExp => st.mkDomainD(new NullValue)
      case ve: VoidExp => st.mkDomainD(new VoidValue)
      
      case re@(RegisterExp(sv)) => {
       val regAddr =  fp.offset(re.regStr)
       storeLookup(st,regAddr)
      }
      
      // For string exp evaluation
      case sle@StringLitExp(_) => {
       st.mkDomainD(StringLit(sle.strLit))
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
  
   def isMoveResult(st: Stmt) : (Boolean, List[RegisterExp]) = {
	   st match {
	   case ass@AssignAExpStmt(lhReg, rhExp, nxt, ls, clsP, metP) => {
	    val targetRegExp= 
	     lhReg match {
        case RegisterExp(sv) => {
          lhReg.asInstanceOf[RegisterExp]
        }
        case _ => {
          throw new SemanticException("ASsignAexpStmt: left hand side is not  RegisterExp" + lhReg.toString())
        }
      }
		   rhExp match {
		   case RegisterExp(sv) => { // should be move-result 
			   val srcReg = rhExp.asInstanceOf[RegisterExp]
			    (true, List(targetRegExp)) 
		   }
		   case _ => (false, List())
		   }
	   }
	   case _ => (false,List()) 
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
  
  def filterObjValues(objs: D, s: Store) : D = {//Set[ObjectValue] = {
    val vs = 
      objs.toList filter ((o: Value) => o match {
      case ob@(ObjectValue(_,_)) => true
     // case ObjectSomeTop(_) => true // should we need this at all? yes. we need to pass the topObject around.
      case _ => false
    })
  
     s.mkDomainD(vs.map(_.asInstanceOf[ObjectValue]): _*)
   /*vs.map ((s)=>{
     s match {
       case ob@(ObjectValue(_,_)) => {s.asInstanceOf[ObjectValue]}
       case ObjectSomeTop(_) => {s.asInstanceOf[ObjectSomeTop]}
     }
   })*/
  }
  
  // for not null to detect object valuess, only need to return Value type
  def filterAbsObjAndStringTop(objs: Set[Value]) : Set[Value] = {
    objs.filter((obj) => {
      obj match {
        case ObjectValue(_, _) | ObjectSomeTop(_) |  StringTop | StringLit(_)   => true
        case _ => false
      }
    })
  }
  
  
  def filterStrObjVals(objVals: D, s: Store) : D ={
    s.mkDomainD(objVals.toList.filter(_.asInstanceOf[ObjectValue].className == "java/lang/String"): _*)
  }
  
  def filterStrBuilderObjVals(objVals: D, s: Store) : D ={
    s.mkDomainD(objVals.toList.filter(_.asInstanceOf[ObjectValue].className == "java/lang/StringBuilder") : _*)
  }
  
  def filterClassObjVals(objVals: D, s: Store) : D ={
     s.mkDomainD(objVals.toList.filter(_.asInstanceOf[ObjectValue].className == "java/lang/Class"): _*)
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
        // println("fields of clsName: " + clsName, fields)
      fields
      }
      case None => {
        List()
      }
    }
  }
   
    def getRegExpStr(ae : AExp) : String = {
                ae match {
                  case re@RegisterExp(_) => {
                    re.toString
                  }
                  case _ => {
                    throw new Exception("@handleExternalLibCalls")
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
  
    val ps = fieldOffsets map ((_, s.mkDomainD()))//Set[Value]()))
    //val res = storeUpdate(s, ps)// strong update
    storeStrongUpdate(s,ps)
    //res
  }
  
  def initObjectProperty(classPath: String, pst: PropertyStore, op: ObjectPointer, securityValues: D //Set[Value]
  ) : PropertyStore ={
     val fieldPathStrs = getFieldTypeStrs(classPath)
    val fieldOffsets = fieldPathStrs.map {
      case (path, ftype) => op.offset(path)
    }
    
    val ps = fieldOffsets map ((_, securityValues))
    //storeUpdate(pst, ps) //strong update
   
     storeStrongUpdate(pst,ps) 
  }
  
  
   def propagatePStore(pst: PropertyStore, strName: String, stForEqual: StForEqual,  targetAddrs: List[Addr ], strongUpdate: Boolean) : PropertyStore = {
	   val sourceOrSinkLevel = stForEqual.oldStyleSt.sourceOrSink //DalInformationFlow.decideSourceOrSinkLevel(strName) 
	    // println(strName + "sourceOrsink" + sourceOrSinkLevel)
	   if(sourceOrSinkLevel>0) {
	    // println(strName + "sourceOrsink" + sourceOrSinkLevel)
		   //val securityValue = SecurityValue(stForEqual.clsPath, stForEqual.methPath, stForEqual.lineSt, strName, sourceOrSinkLevel)
		   //val bindings = targetAddrs.map((_, Set(securityValue.asInstanceOf[Value])))
	     val bindings = targetAddrs.map((_, pst.mkDomainD(genTaintKindValueFromStmt(stForEqual.oldStyleSt, pst).toList: _*)))
		   if(strongUpdate) {
		     // please just use strong update!
		      //storeStrongUpdate(pst, bindings) 
		      storeUpdate(pst, bindings) 
		   }
		       
		   else
		     storeUpdate(pst, bindings) 
	   }else{
		   pst
	   }

   }
  
   
              
 
   def canHaveEmptyContinuation(c: ControlState) = c match {
    case FinalState(_) => true
    case ErrorState(_, _, _) => true
    /**
     * Be careful with the following!
     * when can the parital state have empty continuation?
     * Currnently NO?
     */
    case PartialState(st, fp, store, ps, kptr, t) => true
    //  if isAtomic(ae) => true
    case _ => false
  }

  def mustHaveOnlyEmptyContinuation(c: ControlState) = c match {
    case FinalState(_) => true
    case _ => false
  }
  
  
  def collectPerms(apiName: String) {
    val gPermMap = Thread.currentThread().asInstanceOf[AnalysisHelperThread].permissionMap 
      if(gPermMap.contains(apiName)){ 
    	  val pp = gPermMap get apiName
    	  pp match{
    	  case Some(p) => { 
    		  p.isAccessed = true 
    		  Thread.currentThread().asInstanceOf[AnalysisHelperThread].permissionMap(apiName) = p 
    	  }
    	  //case None => gPermMap
      }} 
  } 
  
  
   def updateHeatMap(st: StForEqual) {
     
     val curHeatMap = Thread.currentThread().asInstanceOf[AnalysisHelperThread].heatMap
     if(curHeatMap.contains(st)) {
       val curCountO = curHeatMap get st
       val newHeatPair = curCountO match {
         case Some(hp) => HeatPair(hp.cnt + 1)
         case None =>  HeatPair(0)
       } 
       curHeatMap (st) = newHeatPair
     }else {
       curHeatMap (st) = HeatPair(0)
     }
   }
   
   def getTypeAndValForArgs(tyStrs: List[String], argVals: List[D], store:Store) : Set[(String, Set[String])] = {
     println("args: ", tyStrs)
     val argValsList = argVals.map((argVald) => argVald.toList.toSet)
     val typeVals = tyStrs.zip(argValsList)
     typeVals.foldLeft(Set[(String, Set[String])]())((res, vs) => {
       
      val innerRes = 
       vs._2.foldLeft(Set[(String, Set[String])]())((res2, v) => {
       v match {
      
        case av =>{
         av match{
           case ObjectValue(op, clsName) => {
             if(clsName == "java/lang/String"){
               val stringAddr = op.offset("value")
               val resStrLits = storeLookup(store, stringAddr)
               val resStrLits2  = resStrLits.toSet.filter((resStr) =>{
                 resStr match{
                   case StringLit(str) =>  true
                   case _=>  false
                 }
               })
               val resStrss =resStrLits2.map(_.asInstanceOf[StringLit]).map((strlit) => strlit.str)
              res2 ++ Set(( vs._1, resStrss))
             }else res
           }
           case TrueValue() => {Set(("bool", Set("true"))) ++ res2}
           case FalseValue() => {  res2 ++ Set(("bool", Set("false")))}
           case BoolTop => {Set(("bool", Set("top"))) ++ res2}
           
           case NumLit(bi) => Set((vs._1, Set(bi.toString))) ++ res2
           case NumTop => Set((vs._1, Set("Numeric Top"))) ++ res2
           case _ => {res2}
         }    }
        
      //  case _ => {throw new Exception("@getTypeAndValsForArgs: not a pair")}
       }
     })
     
     innerRes ++ res
     })
   }
   
    def getPossibleStrings(vals : D, store: Store) : Set[String] = {
       vals.toSet.foldLeft(Set[String]())((res, v) => {
         v match{
           case ObjectValue(op, clsName) => {
             if(clsName == "java/lang/String"){
               val stringAddr = op.offset("value")
               val resStrLits = storeLookup(store, stringAddr)
               val resStrLits2  = resStrLits.toSet.filter((resStr) =>{
                 resStr match{
                   case StringLit(str) =>  true
                   case _=>  false
                 }
               })
               resStrLits2.map(_.asInstanceOf[StringLit]).map((strlit) => strlit.str) ++ res
             }else res
           }
           case _ => res
         }
       })
     }

  class CESKException(s: String) extends SemanticException(s)
}