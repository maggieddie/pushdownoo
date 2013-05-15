package org.ucombinator.dalvik.parsing

import org.ucombinator.dalvik.syntax._
import scala.collection.immutable.{ Set => ImmSet, Map => ImmMap }
import scala.collection.mutable.Map
import org.ucombinator.dalvik.syntax.DalvikClassDef
import org.ucombinator.dalvik.syntax.SText
import org.ucombinator.utils._
import org.ucombinator.dalvik.syntax.DalvikClassDef

class S2DParser {

  import CommonSSymbols._;

  private def parseClassDef //(fieldTable : ImmMap[SName, FieldDef], MethodTable: ImmMap[SName, MethodDef])
  (sx: SExp): Option[DalvikClassDef] = {
    import org.ucombinator.dalvik.syntax.SText
   
    def parseSuperOrSource(sx: SExp): Option[SName] = {
      sx match {
        case SSuper :+: (sun: SName) :+: SNil() => Some(sun)
        case _ => None
      }
    }
    sx match {
      case SClass :+: attrs    
        :+: (clsName: SName)
        :+: supercons
        :+: source
        :+: fieldsAndMethods => {
           val clsClsStr = clsName.toString()
           // no parsing of the support lib 
          if(clsClsStr.startsWith("android/support/v4")) {
            None
          }
          else{
        val supSn = CommonUtils.TestSNRet(parseSuperOrSource(supercons))
        val (flds, meths, interfaceNames, interfaces) = parseClassBody(List(), List(), List(), Map.empty)(fieldsAndMethods.toList,  clsName.toString())
       
        val dvcd = new DalvikClassDef(clsClsStr, supSn, flds, meths, interfaceNames, interfaces)
        dvcd.registerClass(clsClsStr)
       // Debug.prntDebugInfo("one class test", DalvikClassDef.forName(clsClsStr))
        Some(dvcd)
          }
      }
       case SInterface :+: attrs
        :+: (clsName: SName)
        :+: supercons
        :+: source
        :+: fieldsAndMethods => {
        val supSn = CommonUtils.TestSNRet(parseSuperOrSource(supercons))
       /* val (flds, meths) = parseFieldsMethods(List(), List())(fieldsAndMethods.toList)
        Debug.prntDebugInfo("meths ", meths)
        val clsClsStr = clsName.toString()
        val dvcd = new DalvikClassDef(clsClsStr, supSn, flds, meths)
        dvcd.registerClass(clsClsStr)
        Debug.prntDebugInfo("one class test", DalvikClassDef.forName(clsClsStr))*/
        None // Dont deal with INterface yet
      } 
    }
  }

  private def parseField(sx: SExp, attrs: List[String], clsP: String, isStatic: Boolean): FieldDef = {
    if (isStatic) {
      sx match {
        //case (fieldName: SName) :+: (fieldType: SExp) :+: value :+: SNil() => {
         case (fieldName: SName) :+: rest => { 
          //val valuee = ParsingUtils.toAExp(value)
          //FieldDef(fieldName.toString(), attrs, fieldType.toString())
          FieldDef(StringUtils.getDistinctMethodOrFieldPath(clsP, fieldName.toString, "fld"), attrs, rest.toString)
        }
        //case _ => Any
      }

    } else {
      sx match {
        case (fieldName: SName) :+: rest => { // :+: (fieldType: SName) => { //:+: SNil() => {
          FieldDef(fieldName.toString, attrs, rest.toString)//StringUtils.trimLRPars(rest.toString()))
        }
      }
    }
  }

  private def parseClassBody(fields: List[FieldDef], methods: List[MethodDef] , interfaceNames: List[String], implmentedInterfaces: Map[CompactMethodIndex, MethodDef])(sxl: List[SExp], clsP:String): (List[FieldDef], List[MethodDef] , List[String], Map[CompactMethodIndex, MethodDef]) = {
    sxl match {
      case Nil => {
        (fields, methods,interfaceNames, implmentedInterfaces )
      }
      case hd :: tl => {
        hd match {
          case SField :+: attrs :+: rest => {
            val attrList = parseAttrs(attrs)
            if (attrList.length == 0) { // this is the case where the field does not have the acces flags in the SExp
              Debug.prntDebugInfo("NONONO attrs modifier", hd)
              hd match {
                case SField :+: attrs :+: rest => {
                  val newField = parseField(rest, List(), clsP,false)
                  parseClassBody(newField :: newField :: fields, methods, interfaceNames, implmentedInterfaces)(tl, clsP) // bug? why the heck newFIeld added twice
                }
              }
            } else {
              val isStatic = flagsContains(attrList, "static")
              val attrsStrs = StringUtils.toLstStrFromLstSExp(attrList)
              val newField = parseField(rest, attrsStrs, clsP, isStatic)
              parseClassBody(newField :: newField :: fields, methods, interfaceNames, implmentedInterfaces)(tl, clsP)
            }
          }
          // the implmented interfaces, we just get the name anyway. 
          case SImpl :+: (interfaceName: SName) :+: SNil() => {
            
            parseClassBody( fields, methods, interfaceName.toString::interfaceNames, implmentedInterfaces)(tl,clsP)
          }
          // The method match!!
          case _ =>
            val methDef = parseMethod(hd, clsP)
            parseClassBody(fields, methDef  :: methods, interfaceNames, implmentedInterfaces)(tl, clsP)
          //fields
        }
      }
    }
  }

  private def parseMethod(sx: SExp, clsP: String): MethodDef = {
    sx match {
      // method head, not yet throws? later
      case SMethod :+: attrs :+: (methodName: SName) :+: formalTypes :+: (retType: SExp) :+: body => {
        val attrList = parseAttrs(attrs) map (_.toString())
        val methName = methodName.toString()
        val formalTys = ParsingUtils.sexpElemsToStrList(formalTypes)
        val retTyStr = retType.toString()
        // there must be a (limit registers number) in the first line of the body??)
        val(throwAnnotations, rest1) = parseThrowsAnnotation(body) 
        val (regLimit, rest2) = getRegLimitsFromList(rest1)
        val (handlerLst, rest3) = (getCatchHandlers(rest2, List()), rest2)
        val stmt = parseBodyMap(rest3, clsP, methName)//parseBodyTailRecursion(List(), rest)//parseBody(rest)
        val stmtTransformed = ParsingUtils.transFormBody(stmt, ExceptionHandlers(handlerLst), throwAnnotations, clsP, methName)
        MethodDef(StringUtils.getDistinctMethodOrFieldPath(clsP,methName, "meth"), attrList, regLimit, formalTys, retTyStr, stmt, ExceptionHandlers(handlerLst), throwAnnotations)
      }
      //case other cases- let it fail
    }
  }

  private def getRegLimits(sxl: SExp): (BigInt, List[SExp]) = {
    val sl = sxl.toList
    val hdLimit = sl.head
    hdLimit match {
      case SLimit :+: SRegs :+: (snum: SInt) :+: SNil() => { //(snum : SInt) => {
        val num = StringUtils.strToInt(snum.toString)
        (num, sl.tail)
      }
      case _ => {
       
        (0, sl.tail)
      }
    }
  }

  private def getRegLimitsFromList(sxl: List[SExp]): (BigInt, List[SExp]) = {
    if (sxl.isEmpty) (-99999, List())
    else {
      val hdLimit = sxl.head
      hdLimit match {
        case SLimit :+: SRegs :+: (snum: SInt) :+: SNil() => { //(snum : SInt) => {
          val num = StringUtils.strToInt(snum.toString)
          (num, sxl.tail)
        }
        case _ => {
          (0, sxl)
        }
      }
    }
  }
  //SSystemVisibility, SAnnotationValue
  //(annotation systemVisibility [object dalvik/annotation/Throws]
  // (value [array [object java/lang/String]] ( [object java/io/FileNotFoundException]  [object java/io/IOException] ))
  //)
  private def parseThrowsAnnotation(sxl: SExp): (List[String], List[SExp]) = {
    val sl = sxl.toList
    if (sl.isEmpty) {
      (List(), List())
    } else {
      val hdAnn = sl.head
      hdAnn match {
        case SAnnotation :+: SSystemVisibility :+: rest => { //(snum : SInt) => {
          val reststl = rest.toList
          val anntype = reststl.head
          if (anntype.toString() == "(object dalvik/annotation/Throws)") {
            val annVals = parseAnnotationValue(reststl.tail.head)
            (annVals, sl.tail)
          } // for toher annotation types, we don't parse
          else (List(), sl.tail)

        }
        case _ => {
          (List(), sl)
        }
      }
    }
  }
  
  private def parseAnnotationValue(sx: SExp) : List[String] = {
  
    sx match {
      case SAnnotationValue :+: restValues => {
        val restValsObjWrappers = restValues.toList.tail
      
        restValsObjWrappers.map((objW) => StringUtils.getTypeFromObjectWrapper(objW.toString))
      }
      case _ => throw new Exception("cant mactch annotation values" + sx)
    }
  }
  
  /**
   * We intentionally let the handler's order not reverse!
   *  so that the pushed handler will be in the reverse order, 
   *  and the subtype will be pushed later.
   *  and so when throw sees the pushed handler in the right order too
   *  To Test Right
   */
  private def getCatchHandlers(sxl: List[SExp], res: List[ExceptionHandler]) : List[ExceptionHandler] = {
    sxl match {
      case Nil => {
      	 res
      }
      case hd :: tl => {
        hd match {
          case SCatch :+: (exnName: SName) :+: SFrom :+: (startingLabel: SName)
            :+: STo :+: (endLabel: SName)
            :+: SUsing :+: (handlerLabel: SName)
            :+: SNil() => {
             val hd =  ExceptionHandler("normal", exnName.toString(),
              startingLabel.toString(),
              endLabel.toString(),
              handlerLabel.toString())
              getCatchHandlers(tl, hd :: res)
            }
            
            case SCatchAll :+: (exnName: SName) :+: SFrom :+: (startingLabel: SName)
            :+: STo :+: (endLabel: SName)
            :+: SUsing :+: (handlerLabel: SName)
            :+: SNil() => {
                val hd =  ExceptionHandler("finally", exnName.toString(),
              startingLabel.toString(),
              endLabel.toString(),
              handlerLabel.toString())
              getCatchHandlers(tl, hd :: res)
            }
            case _ =>  getCatchHandlers(tl, res)
        }
      }
    }
  }
  
  
  // see this willl cause stack overflow?
  private def setLineNumber(sl: List[Stmt], curLineS: Stmt, res: List[Stmt]) : List[Stmt] ={
    sl match {
      case Nil => res.reverse
      case hd :: tl => {
        hd match {
          case  LineStmt(_, _,_,_, _) => {
            setLineNumber(tl, hd, hd :: res)
          }
          case StmtNil => setLineNumber(tl, hd, hd:: res)
          case _ => {
            hd.lineNumber = curLineS
            setLineNumber(tl, curLineS, hd :: res)
          }
        }
        
      }
    }
  }

  private def parseBodyMap(sl: List[SExp], clsPath: String, methPath: String): Stmt = {
// if is just empty body, then return empty stmt
    if (sl.isEmpty) StmtNil
    else {
      val lstS = sl.map((s) => {parse(s, clsPath, methPath)})
      val lstS2 = CommonUtils.extractStmts(lstS)

      val lst = setLineNumber(lstS2, StmtNil, List())

      val linkHeadO =
        CommonUtils.linkedListWrapper(List())(lst)
      val linkHead =
        linkHeadO match {
          case Some(s) => s
          case None => { StmtNil }
        }
      val flatlist = CommonUtils.flattenLinkedStmt(List())(linkHead)

      ParsingUtils.updateLabelStmts(linkHead)
      linkHead
    }
  }

  
  
  private def parse(sx : SExp, clsPath:String, methPath: String) : Option[Stmt] ={
    sx match{
            case SNop :+: SNil() => {
           Some( NopStmt(StmtNil, StmtNil, clsPath, methPath))
          }
          case SLabel :+: (lineNo: SName) :+: SNil() => {
           val  lblSt = new LabelStmt(lineNo.toString(), StmtNil, StmtNil, clsPath, methPath)
           lblSt.register(lineNo.toString())
           Some( lblSt)
           
          }
          
          case SLine :+: (snum: SInt) :+: SNil() =>{
            Some (new LineStmt(snum.toString(), StmtNil, StmtNil, clsPath, methPath))
          }
          
          case SCatch :+: (exnName: SName) :+: SFrom :+: (startingLabel: SName)
            :+: STo :+: (endLabel: SName)
            :+: SUsing :+: (handlerLabel: SName)
            :+: SNil() => {
             Some( CatchStmt("normal", exnName.toString(),
              startingLabel.toString(),
              endLabel.toString(),
              handlerLabel.toString(), StmtNil, StmtNil, clsPath, methPath))
             
            }
            
            case SCatchAll :+: (exnName: SName) :+: SFrom :+: (startingLabel: SName)
            :+: STo :+: (endLabel: SName)
            :+: SUsing :+: (handlerLabel: SName)
            :+: SNil() => {
             Some( CatchStmt("finally", exnName.toString(),
              startingLabel.toString(),
              endLabel.toString(),
              handlerLabel.toString(), StmtNil, StmtNil,clsPath, methPath))
             
            }
          case SThrow :+: (sreg: SName) :+: SNil() => {
            Some(ThrowStmt(RegisterExp(sreg), StmtNil, StmtNil,clsPath, methPath))
           
          }
          
          //case class InvokeStmt(methPathStr: String,argRegAExp: List[AExp], objAExp:AExp, tyStr: List[String], nxt: Stmt) 
          case SInvokeDirect :+: argsArgs :+: (methPPath: SName) :+: types => {
            Some(ParsingUtils.genInvokeStmt(methPPath, argsArgs, types, StmtNil, clsPath, methPath,  1, false))
           
          }
          case SInvokeVirtual :+: argsArgs :+: (methPPath: SName) :+: types => {
            
            Some(ParsingUtils.genInvokeStmt(methPPath, argsArgs, types, StmtNil,clsPath, methPath,  2, false))
            
          }
          case SInvokeSuper :+: argsArgs :+: (methPPath: SName) :+: types => {
            
            Some(ParsingUtils.genInvokeStmt(methPPath, argsArgs, types, StmtNil, clsPath, methPath,3, false))
           
          }
          case SInvokeStatic :+: argsArgs :+: (methPPath: SName) :+: types => {
            
            Some(ParsingUtils.genInvokeStmt(methPPath, argsArgs, types, StmtNil,clsPath, methPath, 0, false))
           
          }
        
          case SInvokeInterface :+: argsArgs :+: (methPPath: SName) :+: types => {
            Some(ParsingUtils.genInterfaceInvokeStmt(methPPath, argsArgs, types, StmtNil, clsPath, methPath, false))
          }
          
          //invoke-kind/range
          case SInvokeDirectRange :+: argsArgs :+: (methPPath: SName) :+: types => {
            Some(ParsingUtils.genInvokeStmt(methPPath, argsArgs, types, StmtNil, clsPath, methPath,1, true))
           
          }
          case SInvokeVirtualRange :+: argsArgs :+: (methPPath: SName) :+: types => {
            
            Some(ParsingUtils.genInvokeStmt(methPPath, argsArgs, types, StmtNil,clsPath, methPath, 2, true))
            
          }
          case SInvokeSuperRange :+: argsArgs :+: (methPPath: SName) :+: types => {
            
            Some(ParsingUtils.genInvokeStmt(methPPath, argsArgs, types, StmtNil, clsPath, methPath,3, true))
           
          }
          case SInvokeStaticRange :+: argsArgs :+: (methPPath: SName) :+: types => {
            
            Some(ParsingUtils.genInvokeStmt(methPPath, argsArgs, types, StmtNil,clsPath, methPath, 0, true))
           
          }
        
          case SInvokeInterfaceRange :+: argsArgs :+: (methPPath: SName) :+: types => {
            Some(ParsingUtils.genInterfaceInvokeStmt(methPPath, argsArgs, types, StmtNil, clsPath, methPath, true))
          }
          
          
          // TODO invoke ranges

          //return
          case SReturnVoid :+: SNil() => {
            Some(new ReturnStmt(RegisterExp(SName.from("")), StmtNil, StmtNil, clsPath, methPath))
            
          }
          case SReturn :+: (resReg: SName) :+: SNil() => {
            Some(new ReturnStmt(RegisterExp(resReg), StmtNil, StmtNil, clsPath, methPath))
           
          }
          case SReturnWide :+: (resReg: SName) :+: SNil() => {
            Some(new ReturnStmt(RegisterExp(resReg), StmtNil, StmtNil, clsPath, methPath))
            
          }
          case SReturnObject :+: (resReg: SName) :+: SNil() => {
            Some( new ReturnStmt(RegisterExp(resReg), StmtNil, StmtNil, clsPath, methPath))
             
          }

          // const
          case SConst4 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SConst4, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SConst16 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SConst16, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SConst :+: rest => {
            Some(ParsingUtils.genAssignStmt(SConst, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SConstHigh16 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SConstHigh16, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SConstWide32 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SConstWide32, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SConstWide :+: rest => {
             Some(ParsingUtils.genAssignStmt(SConstWide, rest, StmtNil, clsPath, methPath, false))
             
          }

          case SConstWideHigh16 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SConstWideHigh16, rest, StmtNil, clsPath, methPath, false))
            

          }
          case SConstString :+: rest => {
            Some(ParsingUtils.genAssignStmt(SConstString, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SConstStringJumbo :+: rest => {
            val st = ParsingUtils.genAssignStmt(SConstStringJumbo, rest, StmtNil, clsPath, methPath, false)
            Some(st)
            
          }

          case SConstClass :+: rest => {
            Some(ParsingUtils.genAssignStmt(SConstClass, rest, StmtNil, clsPath, methPath, false))
           
          }
          
           case SCheckCast :+: rest => {
            Some(ParsingUtils.genAssignStmt(SCheckCast, rest, StmtNil, clsPath, methPath, false))
           
          }

          // new-instance
          case SNew :+: (resReg: SName) :+: clsType :+: SNil() => {
            Some(ParsingUtils.genNewStmt(resReg, clsType, StmtNil, clsPath, methPath))
           
          }
           // iget
          case SIGet :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType=> {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg, objReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
           
          }
            
          case SIGetWide :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg, objReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
           
          }
          case SIGetObject :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType=> {
        	  	Some(ParsingUtils.genNSGetOrPutStmt(destReg, objReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
        	  
          }
            
          case SIGetByte :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg, objReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
           
          }
            
          case SIGetChar :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg, objReg, fieldPath, fldType, StmtNil, clsPath, methPath,true))
           
          }
            
          case SIGetShort :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg, objReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
           
          }
            
          case SIGetBoolean :+: (destReg: SName) :+:  (objReg:SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg, objReg,fieldPath, fldType, StmtNil, clsPath, methPath, true))
           
          }
    
         //iput
          case SIPut :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType =>{
             Some(ParsingUtils.genNSGetOrPutStmt(destReg, objReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
            
          }
          case SIPutWide :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType=> {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg,  objReg,fieldPath, fldType, StmtNil, clsPath, methPath, false))
           
          }
            
          case SIPutObject :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg,  objReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
           
          }
          case SIPutBoolean :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+:fldType => {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg,  objReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
            
          }
          case SIPutByte :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genNSGetOrPutStmt(destReg,  objReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
           
          }
          case SIPutChar :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType => {
             Some(ParsingUtils.genNSGetOrPutStmt(destReg,  objReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
            
          }
           case SIPutShort :+: (destReg: SName) :+: (objReg:SName) :+: (fieldPath: SName) :+: fldType => {
              Some(ParsingUtils.genNSGetOrPutStmt(destReg,  objReg, fieldPath, fldType, StmtNil, clsPath, methPath,false))
            
           }
           

          // sget
          case SGet :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType=> {
            Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
            
          }
          case SGetWide :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType => {
             Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
            
          }
          case SGetObject :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType => {
             val sto = ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, true)
             Debug.prntDebugInfo("sget object", sto)
            Some(sto)
           
          }
          case SGetByte :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType=> {
             Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
           
          }
          case SGetChar :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
           
          }
          case SGetShort :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType => {
             Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
           
          }
           
          case SGetBoolean :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, true))
           
          }
          //sput
          case SPut :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
            
          }
          case SPutWide :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType=> {
             Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath,  false))
            
          }
          case SPutObject :+: (destReg: SName) :+: (fieldPath: SName)  :+: fldType => {
             Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
           
          }
          case SPutBoolean :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
           
          }
          case SPutByte :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType => {
            Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
           
          }
          case SPutChar :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType=> {
            Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath,false))
           
          }
            
           case SPutShort :+: (destReg: SName) :+: (fieldPath: SName) :+: fldType => {
              Some(ParsingUtils.genSGetOrPutStmt(destReg, fieldPath, fldType, StmtNil, clsPath, methPath, false))
              
           }
           

          //move-result
          case SMoveResult :+: (resReg: SName) :+: SNil() => {
           Some(ParsingUtils.genMoveRestultStmt(resReg, StmtNil, clsPath, methPath))
           
          }
          case SMoveResultWide :+: (resReg: SName) :+: SNil() => {
            Some(ParsingUtils.genMoveRestultStmt(resReg, StmtNil, clsPath, methPath))
          }
          case SMoveResultObject :+: (resReg: SName) :+: SNil() => {
            Some(ParsingUtils.genMoveRestultStmt(resReg, StmtNil, clsPath, methPath))
           
          }

          // move-exceptions
          case SMoveException :+: (resReg: SName) :+: SNil() => {
            Some( new MoveExceptionStmt(RegisterExp(resReg), StmtNil, StmtNil, clsPath, methPath))
             
          }

          //case class IfStmt(condExp: AExp, sucLabel:String, nxt: Stmt) extends Stmt {
          // How to delete those duplications
          case SIfEq :+: (sreg: SName) :+: (sreg2: SName) :+: (lbl: SName) :+: SNil() => {
             Some(ParsingUtils.genIfStmt(SIfEq, sreg, sreg2, lbl, StmtNil, clsPath, methPath))
            
          }
          case SIfNe :+: (sreg: SName) :+: (sreg2: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfNe, sreg,  sreg2, lbl, StmtNil, clsPath, methPath))
            
          }
          case SIfLt :+: (sreg: SName) :+: (sreg2: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfLt, sreg,  sreg2, lbl, StmtNil, clsPath, methPath))
            
          }
          case SIfGe :+: (sreg: SName) :+: (sreg2: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfGe, sreg,  sreg2, lbl, StmtNil, clsPath, methPath))
           
          }
          case SIfGt :+: (sreg: SName) :+: (sreg2: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfGt, sreg,  sreg2, lbl, StmtNil, clsPath, methPath))
           
          }
          case SIfle :+: (sreg: SName) :+: (sreg2: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfle, sreg,  sreg2,lbl, StmtNil, clsPath, methPath))
           
          }
          case SIfEqz :+: (sreg: SName) :+: (lbl: SName):+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfEqz, sreg, SName.from(""), lbl, StmtNil, clsPath, methPath))
            
          }
          case SIfNez :+: (sreg: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfNez,sreg, SName.from("") ,lbl, StmtNil, clsPath, methPath))
           
          }
          case SIfle :+: (sreg: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfNez, sreg, SName.from("") ,lbl, StmtNil, clsPath, methPath))
           
          }
          case SIfLtz :+: (sreg: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfLtz, sreg, SName.from("") ,lbl, StmtNil, clsPath, methPath))
           
          }
          case SIfGez :+: (sreg: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfGez, sreg, SName.from("") ,lbl, StmtNil, clsPath, methPath))
            
          }
          case SIfGtz :+: (sreg: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfGtz, sreg, SName.from("") ,lbl, StmtNil, clsPath, methPath))
            
          }
          case SIfLez :+: (sreg: SName) :+: (lbl: SName) :+: SNil() => {
            Some(ParsingUtils.genIfStmt(SIfLez, sreg,  SName.from("") , lbl, StmtNil, clsPath, methPath))
           
          }
          
          case SPackedSwitch :+: (sreg: SName) :+: (soffset: SInt) :+: lbls =>{
            val smt = ParsingUtils.genSwitchStmt(sreg, soffset, lbls, StmtNil, clsPath, methPath)
            //println("the packed generated switch result is: \n " , smt )
            Some(smt)
          }
          case SSparseSwitch :+: (sreg: SName) :+: lbls => {
            val smt= ParsingUtils.genSwitchStmt(sreg, SInt(-9999), lbls, StmtNil, clsPath, methPath)
            //println("the sparse generated switch result is: \n " , smt )
            Some(smt)
          }
          
          case SGoto :+: (lbl: SName) :+: SNil() => {
            Some(new GotoStmt(lbl.toString(), StmtNil, StmtNil, clsPath, methPath))
           
          }
          case SGoto16 :+: (lbl: SName) :+: SNil() => {
            Some(new GotoStmt(lbl.toString(), StmtNil, StmtNil, clsPath, methPath))
            
          }
          case SGoto32 :+: (lbl: SName) :+: SNil() => {
           Some( new GotoStmt(lbl.toString(), StmtNil, StmtNil, clsPath, methPath))
           
          }
          //
          case SCmplFloat :+: rest => {
            Some(ParsingUtils.genAssignStmt(SCmplFloat, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SCmpgFloat :+: rest => {
            Some(ParsingUtils.genAssignStmt(SCmpgFloat, rest, StmtNil, clsPath, methPath,false))
           
          }
          case SCmplDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SCmplDouble, rest, StmtNil, clsPath, methPath,false))
            
          }
          case SCmpgDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SCmpgDouble, rest, StmtNil,clsPath, methPath, false))
            
          }
          case SCompLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SCompLong, rest, StmtNil, clsPath, methPath,false))
           
          }

          //move
          case SMove :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMove, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SMoveFrom16 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMoveFrom16, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SMove16 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMove16, rest, StmtNil, clsPath, methPath, false))
            
          }

          //problem of move wide?
          case SMoveWide :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMoveWide, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SMoveWideFrom16 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMoveWideFrom16, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SMoveObject :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMoveObject, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SMoveWide16 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMoveWide16, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SMoveObjectFrom16 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SMoveObjectFrom16, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SMoveObject16 :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMoveObject16, rest, StmtNil, clsPath, methPath, false))
            
          }

          //biop
          case SAddInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAddInt, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SSubInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SSubInt, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SMulInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMulInt, rest, StmtNil, clsPath, methPath,false))
            
          }
          case SDivInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDivInt, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SRemInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SRemInt, rest, StmtNil, clsPath, methPath,false))
           
          }
          case SAndInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAndInt, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SOrInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SOrInt, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SXorInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SXorInt, rest, StmtNil,clsPath, methPath,  false))
           
          }
          case SShlInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SShlInt, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SShrInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SShrInt, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SUShrInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SUShrInt, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SAddLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAddLong, rest, StmtNil,clsPath, methPath,  false))
            
          }
          case SSubLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SSubLong, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SMulLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMulLong, rest, StmtNil,clsPath, methPath,  false))
            
          }
          case SDivLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDivLong, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SRemLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SRemLong, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SAndLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAndLong, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SOrLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SOrLong, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SXorLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SXorLong, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SShlLong :+: rest => {
           Some(ParsingUtils.genAssignStmt(SShlLong, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SShrLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SShrLong, rest, StmtNil,clsPath, methPath,  false))
           
          }
          case SUShrLong :+: rest => {
             Some(ParsingUtils.genAssignStmt(SUShrLong, rest, StmtNil, clsPath, methPath, false))
             
          }
          case SAddFloat :+: rest => {
             Some(ParsingUtils.genAssignStmt(SAddFloat, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SAddFloat :+: rest => {
           Some(ParsingUtils.genAssignStmt(SAddFloat, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SSubFloat :+: rest => {
            Some(ParsingUtils.genAssignStmt(SSubFloat, rest, StmtNil,clsPath, methPath, false))
            
          }
          case SMulFloat :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMulFloat, rest, StmtNil,clsPath, methPath,  false))
           
          }
          case SDivFloat :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDivFloat, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SRemFloat :+: rest => {
            Some(ParsingUtils.genAssignStmt(SRemFloat, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SAddDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAddDouble, rest, StmtNil,clsPath, methPath,  false))
            
          }
          case SSubDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SSubDouble, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SMulDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMulDouble, rest, StmtNil,clsPath, methPath,  false))
           
          }
          case SDivDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDivDouble, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SRemDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SRemDouble, rest, StmtNil,clsPath, methPath,  false))
            
          }

          //uop
          case SNegInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SNegInt, rest, StmtNil,clsPath, methPath,  false))
            
          }
          case SNotInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SNotInt, rest, StmtNil,clsPath, methPath,  false))
           
          }
          case SNegLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SNegLong, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SNotLong :+: rest => {
             Some(ParsingUtils.genAssignStmt(SNotLong, rest, StmtNil,clsPath, methPath,  false))
             
          }
          case SNegFloat :+: rest => {
            Some(ParsingUtils.genAssignStmt(SNegFloat, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SNegDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SNegDouble, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SIntToLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SIntToLong, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SIntToFloat :+: rest => {
            Some(ParsingUtils.genAssignStmt(SIntToFloat, rest, StmtNil,clsPath, methPath,  false))
           
          }
          
          case SIntToDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SIntToDouble, rest, StmtNil,clsPath, methPath,  false))
           
          }
          case SLongToInt :+: rest => {
             Some(ParsingUtils.genAssignStmt(SLongToInt, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SLongToFloat :+: rest => {
            Some(ParsingUtils.genAssignStmt(SLongToFloat, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SLongToDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SLongToDouble, rest, StmtNil,clsPath, methPath,  false))
           
          }
          case SFloatToInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SFloatToInt, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SFloatToLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SFloatToLong, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SFloatToDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SFloatToDouble, rest, StmtNil,clsPath, methPath,  false))
           
          }
          case SDoubleToInt :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDoubleToInt, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SDoubleToLong :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDoubleToLong, rest, StmtNil,clsPath, methPath,  false))
            
          }
          case SDoubleToDouble :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDoubleToDouble, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SIntToByte :+: rest => {
             Some(ParsingUtils.genAssignStmt(SIntToByte, rest, StmtNil, clsPath, methPath, false))
            
          }
          case SIntToChar :+: rest => {
            Some(ParsingUtils.genAssignStmt(SIntToChar, rest, StmtNil, clsPath, methPath, false))
           
          }
          case SIntToShort :+: rest => {
            Some(ParsingUtils.genAssignStmt(SIntToShort, rest, StmtNil, clsPath, methPath, false))
           
          }

          //biop/adde2
          case SAddInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAddInt2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SSubInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SSubInt2Addr, rest, StmtNil,clsPath, methPath,  true))
           
          }
          case SMulInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMulInt2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SDivInt2Addr :+: rest => {
             Some(ParsingUtils.genAssignStmt(SDivInt2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SRemInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SRemInt2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SAndInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAndInt2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SOrInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SOrInt2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SXorInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SXorInt2Addr, rest, StmtNil,clsPath, methPath,  true))
           
          }
          case SShlInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SShlInt2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SShrInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SShrInt2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SUShrInt2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SUShrInt2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SAddLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAddLong2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SSubLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SSubLong2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SMulLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMulLong2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SDivLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDivLong2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SRemLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SRemLong2Addr, rest, StmtNil,clsPath, methPath,  true))
           
          }
          case SAndLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAndLong2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SOrLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SOrLong2Addr, rest, StmtNil, clsPath, methPath, true))
           
          }
          case SXorLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SXorLong2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SShlLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SShlLong2Addr, rest, StmtNil, clsPath, methPath, true))
             
          }
          case SShrLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SShrLong2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SUShrLong2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SUShrLong2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SAddFloat2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAddFloat2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SAddFloat2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAddFloat2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SSubFloat2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SSubFloat2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SMulFloat2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMulFloat2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SDivFloat2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDivFloat2Addr, rest, StmtNil,clsPath, methPath,  true))
            
          }
          case SRemFloat2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SRemFloat2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SAddDouble2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SAddDouble2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SSubDouble2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SSubDouble2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SMulDouble2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SMulDouble2Addr, rest, StmtNil, clsPath, methPath, true))
            
          }
          case SDivDouble2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SDivDouble2Addr, rest, StmtNil,clsPath, methPath,  true))
            
          }
          case SRemDouble2Addr :+: rest => {
            Some(ParsingUtils.genAssignStmt(SRemDouble2Addr, rest, StmtNil,clsPath, methPath,  true))
            
          }
          
          //biop8
          case SAddIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SAddIntLit8, rest, StmtNil, clsPath, methPath, false))
          }
          case SRSubIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SRSubIntLit8, rest, StmtNil, clsPath, methPath, false))
          }
          case SDivIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SDivIntLit8, rest, StmtNil, clsPath, methPath, false))
          }
          case SRemIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SRemIntLit8, rest, StmtNil, clsPath, methPath, false))
          }
          case SAndIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SAndIntLit8, rest, StmtNil, clsPath, methPath, false))
          }
          
          case SOrIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SOrIntLit8, rest, StmtNil, clsPath, methPath, false))
          }
          case SXorIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SXorIntLit8, rest, StmtNil, clsPath, methPath, false))
          }
          
          case SShlIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SShlIntLit8, rest, StmtNil, clsPath, methPath, false))
          }
          case SShrIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SShrIntLit8, rest, StmtNil,clsPath, methPath,  false))
          }
          case SUShrIntLit8 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SUShrIntLit8, rest, StmtNil, clsPath, methPath, false))
          }
    
          //bioplit16
            case SAddIntLit16 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SAddIntLit16, rest, StmtNil, clsPath, methPath, false))
          }
          case SRSubIntLit16 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SRSubIntLit16, rest, StmtNil, clsPath, methPath, false))
          }
          case SMulIntLit16 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SMulIntLit16, rest, StmtNil, clsPath, methPath, false))
          }
          case SDivIntLit16 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SDivIntLit16, rest, StmtNil, clsPath, methPath, false))
          }
          case SRemIntLit16 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SRemIntLit16, rest, StmtNil, clsPath, methPath, false))
          }
          
          case SAndIntLit16 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SAndIntLit16, rest, StmtNil, clsPath, methPath, false))
          }
          case SOrIntLit16 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SOrIntLit16, rest, StmtNil, clsPath, methPath, false))
          }
          
          case SXorIntLit16 :+: rest => {
             Some(ParsingUtils.genAssignStmt(SXorIntLit16, rest, StmtNil, clsPath, methPath, false))
          }
          case _ => None 
            
        
    }
  }
 
  

  

  private def flagsContains(sl: List[SName], kindString: String): Boolean = {
    sl.contains(SName.from(kindString))
  }

  private def parseAttrs(sxl: SExp): List[SName] = {
    sxl match {
      case SAttrs :+: restFlags => {
        ParsingUtils.sexpElemsToList(restFlags)
      }
      case _ => List()
    }
  }

}

object S2DParser {

  def apply(sexps: List[SExp]): Option[DalvikClassDef] = {
    val p = new S2DParser
    
    p.parseClassDef(sexps.head)
  }
}

class ParsingException(str: String) extends Exception 