package org.ucombinator.utils
import org.ucombinator.dalvik.syntax._

object ParsingUtils {
  import CommonSSymbols._;
  // the attrs 
  def sexpElemsToList(sx: SExp): List[SName] = {
    sx.toList map ((n: SExp) => n.asInstanceOf[SName])
  }
  // and the formals
  def sexpElemsToStrList(sx: SExp): List[String] = {
    //   val snLst = sx.toList map ((n: SExp) => n.asInstanceOf[SName])
    //snLst map ((s: SName) => s.toString())
    sx.toList map ((n: SExp) => n.toString)
  }

  def sexpElemsToRegAExpList(sx: SExp): List[AExp] = {
    val snLst = sx.toList map ((n: SExp) => n.asInstanceOf[SName])
    snLst map ((s: SName) => new RegisterExp(s))
  }

  def isLitVal(sx: SExp): Boolean = {
    sx match {
      case i: SInt => true
      case t: SText => true
      case b: SBoolean => true
      case c: SChar => true
      case SVoid => true
      case SNull => true
      // more types to added
      case _ => false
    }
  }

  def litValWrapper(sx: SExp): AExp = {
    sx match {
      case i: SInt => new IntExp(i)
      case t: SText => new StringLitExp(t)
      case b: SBoolean => new BooleanExp(b)
      case c: SChar => new CharLitExp(c)
      case SVoid => new VoidExp
    }
  }
  def toAExp(s: SExp): AExp = {
    if (isLitVal(s)) litValWrapper(s)
    else {
      if(s.isInstanceOf[SName]) {
         val sn = s.asInstanceOf[SName]
         RegisterExp(sn)
      }
      //* OK its's not Sname, maybe type like (array int) or (object a/b/c)
      // but we don't care, just return RegisterExp
      else  RegisterExp(SName.from(s.toString))
    }
  }

  def sexpElemsToAExpList(sx: SExp): List[AExp] = {

    sx.toList map (toAExp(_))
  }

  // it is possible that the number of range of the registers do not match the number of the arg types
  def doMatchInvoke(mp: SName, argRegs: SExp, types: SExp, invokeType:Int, isRange:Boolean): (String, List[AExp], List[String], AExp) = {
    val argRegAes = StringUtils.getArgsFromformals(argRegs, isRange) //ParsingUtils.sexpElemsToRegAExpList(argRegs)
    val typeStrs = ParsingUtils.sexpElemsToStrList(types)
    if(invokeType == 0) { // static only
        (mp.toString(), argRegAes, typeStrs, new NullExp)
    }
    else{
    	val objAe = argRegAes.head
    	val argAes = argRegAes.tail
    	
    	(mp.toString(), argAes, typeStrs, objAe)
    }
  }

  def genInvokeStmt(mp: SName, argRegs: SExp, types: SExp, next: Stmt, clsP:String, methP: String, invokeType: Int, isRange:Boolean): AbstractInvokeStmt = {
    val (methPathStr, argRegAExps, typeStrs, objAe) = doMatchInvoke(mp, argRegs, types, invokeType, isRange )
    invokeType match {
      // static
      case 0 =>  new  InvokeStaticStmt(methPathStr, argRegAExps, typeStrs, next, StmtNil, clsP, methP)
      // direct
      case 1 =>  new InvokeDirectStmt(methPathStr, argRegAExps, objAe, typeStrs, next, StmtNil, clsP, methP)
      
      // virtual
      case 2 => new InvokeStmt(methPathStr, argRegAExps, objAe, typeStrs, next, StmtNil, clsP, methP)
      case 3 => new InvokeSuperStmt(methPathStr, argRegAExps, objAe, typeStrs, next, StmtNil, clsP, methP)
    }
  
  }
  
  // just another gen for invoke interface statment generation.
  def genInterfaceInvokeStmt(mp: SName, argRegs: SExp, types: SExp, next: Stmt, clsP:String, methP:String, isRange:Boolean): AbstractInvokeStmt = {
    val (methPathStr, argRegAExps, typeStrs, objAe) = doMatchInvoke(mp, argRegs, types, 2, isRange)
    
     new InvokeInterfaceStmt(methPathStr, argRegAExps, objAe, typeStrs, next, StmtNil, clsP, methP)
  }

  def genIfStmt(ifOpCode: SName, se: SName, se2: SName, lbl: SName, next: Stmt, clsP: String, methP: String): IfStmt = {
    val regAe = RegisterExp(se)
    val lblStr = lbl.toString()
    ifOpCode match {
      case SIfEqz | SIfNez | SIfle | SIfLtz | SIfGez | SIfGtz | SIfLez =>
        new IfStmt(regAe, lblStr, next, StmtNil, clsP, methP)
      case (if2: SName) => {
        val cond = new AutomicOpExp(if2, List(RegisterExp(se), RegisterExp(se2)): _*)
        new IfStmt(cond, lblStr, next, StmtNil, clsP, methP)
      }
    }
  }
  
  private def parseSparBranches(sxLst: List[SExp]) : List[AExp] ={
    Debug.prntDebugInfo("the to list entries", sxLst)
    if(sxLst.isEmpty) 
      List()
    else{
     
    		sxLst.map((sx: SExp) => {
    		
    		  val sxls = sx.toList 
    		  Debug.prntDebugInfo("the to list entries", sx.toList)
    		  val lbl= sxls(1)
    		  StringLitExp(SText(lbl.toString()))
   	 })
    }
  }
  
  def genSwitchStmt(sreg: SName , soffset: SInt, lbls: SExp, next: Stmt, clsP: String, methP: String) : SwitchStmt = {
   val testAexp = RegisterExp(sreg)
   val lblStrExps = 
    if(soffset.value == -9999) {
      val lstSExp = lbls.toList
      parseSparBranches(lstSExp)
    }else
      sexpElemsToAExpList(lbls) 
    val offsetS = soffset.toString
    new SwitchStmt(testAexp, offsetS, lblStrExps, next, StmtNil, clsP, methP) 
  }

  def genAssignStmt(op: SName, sregs: SExp, next: Stmt, clsP: String, methP: String, isTwoAddr: Boolean): AssignAExpStmt = {
    val operators = sexpElemsToAExpList(sregs)
    val lhs = operators.head
     val rest = 
    if(isTwoAddr) operators else operators.tail
    val rhs = new AutomicOpExp(op, rest: _*)
    new AssignAExpStmt(lhs, rhs, next, StmtNil, clsP, methP)
  }

  def genNewStmt(resReg: SName, clsTye: SExp, next: Stmt, clsP:String, methP: String): NewStmt = {
    val isLit = ParsingUtils.isLitVal(clsTye)
    if (isLit) {
      val classType = ParsingUtils.litValWrapper(clsTye)
      new NewStmt(resReg, classType.toString, next, StmtNil, clsP, methP)
    } else {
      new NewStmt(resReg, clsTye.toString(), next, StmtNil, clsP, methP)
    }
  }

  def genSGetOrPutStmt(destReg: SName, fieldPath: SName, fieldType: SExp, next: Stmt, clsP: String, methP: String, isget: Boolean): FieldAssignStmt = {
    val destRegister = RegisterExp(destReg)
    val fp = fieldPath.toString
    val ft = fieldType.toList.head.toString()
    val fieldExp = new StaticFieldExp(fp, ft)
    if (isget)
      new FieldAssignStmt(destRegister, fieldExp, next, StmtNil, clsP, methP)
    else
      new FieldAssignStmt(fieldExp, destRegister, next, StmtNil, clsP, methP)
  }

  //
  def genNSGetOrPutStmt(destOrSrcReg: SName, objReg: SName, fieldPath: SName, fieldType: SExp, next: Stmt, clsP:String, methP: String, isget: Boolean): FieldAssignStmt = {

    val destOrSrcRegister = RegisterExp(destOrSrcReg)
    val objRegister = RegisterExp(objReg)
    val fp = fieldPath.toString
    val ft = fieldType.toList.head.toString()
    val fieldExp = new NonStaticFieldExp(objRegister, fp, ft)
    if (isget)
      new FieldAssignStmt(destOrSrcRegister, fieldExp, next, StmtNil, clsP, methP)
    else
      new FieldAssignStmt(fieldExp, destOrSrcRegister, next, StmtNil, clsP, methP)
  }


  def genMoveRestultStmt(destReg: SName, next: Stmt, clsP:String, methP:String): AssignAExpStmt = {
    val destRegister = RegisterExp(destReg)
    val srcReg = RegisterExp(SName.from("ret"))
    new AssignAExpStmt(destRegister, srcReg, next, StmtNil, clsP, methP)
  }

  //for transforming the exception
  def extractCatchStmts(fst: Stmt): List[CatchStmt] = {
    val flattenedList = CommonUtils.flattenLinkedStmt(List())(fst)
    Debug.prntDebugInfo("get flattend list: length", flattenedList.length)
    /*    val smts = flattenedList filter ((s: Stmt) => {
      s match {
        case CatchStmt(_, _, _, _, _) => true
        case _ => false
      }
    })*/
    val smts = flattenedList filter ((s: Stmt) => {
      if (s.isInstanceOf[CatchStmt]) true else false
    })
    smts map (_.asInstanceOf[CatchStmt])
  }

  def extractsLabelStmts(fst: Stmt): List[LabelStmt] = {
    val flattenedList = CommonUtils.flattenLinkedStmt(List())(fst)

        val smts = flattenedList filter ((s: Stmt) => {
      s match {
        case LabelStmt(_, _,_, _, _) => true
        case _ => false
      }
    })
    

/*    val smts = flattenedList filter ((s: Stmt) => {
      if (s.isInstanceOf[LabelStmt]) true
      else false
    })*/
    smts map (_.asInstanceOf[LabelStmt])
  }

  def updateLabelStmts(fst: Stmt) {
    val oldLabelStmts = extractsLabelStmts(fst)
    Debug.prntDebugInfo("extracted label after parsing: ", oldLabelStmts.length)
    oldLabelStmts.foreach((ls: LabelStmt) => Stmt.updateLabelWith(ls.label, ls))
  }

  // true is the starting
  def bldStartingOrEndingLblCatches(cts: List[CatchStmt], choice: Boolean): Map[LabelStmt, List[CatchStmt]] = {
    var res: Map[LabelStmt, List[CatchStmt]] = Map.empty
    for (ct <- cts) {
      
      val lbl = choice match {
        case true => ct.from
        case false => ct.to
      }
     
      val lbSt = {
        Stmt.forLabel(lbl) match {
          case Some(l) => {
            l
          }

        }
      }
      if (res.contains(lbSt)) {
        val oldVal = res get lbSt match {
          case Some(l) => l
        }
        res += (lbSt -> (oldVal ::: List(ct)))
      } else
        res += (lbSt -> List(ct))
    }
    res
  }

  def genPushHandlerStmts(catches: List[CatchStmt], exnHandler: ExceptionHandlers, exnAnno: List[String], clsP: String, methP:String): Option[List[Stmt]]= {
    /*val res1 = catches.foldLeft(List[Stmt]())((res, c) => {
       if(catches.indexOf(c) == catches.length - 1){
         val lastPushHanlderSt =  PushHandlerStmt(c.typeStr, c.exceptionType, c.using,  StmtNil, StmtNil)
         val faultInjectorSt = FaultInjectorStmt(List(exnHandler), exnAnno, StmtNil, StmtNil)
         res::: List(lastPushHanlderSt, faultInjectorSt)
       }
        
      else 
         res ::: List(PushHandlerStmt(c.typeStr, c.exceptionType, c.using,  StmtNil, StmtNil))
    })*/
    val res1 = catches map ((c: CatchStmt) => {
      //if it is the alst the push handler
      if(catches.indexOf(c) == catches.length - 1)
         PushHandlerStmt(c.typeStr, c.exceptionType, c.using, true, List(exnHandler),exnAnno,  StmtNil, StmtNil, clsP,methP)
      else 
         PushHandlerStmt(c.typeStr, c.exceptionType, c.using, false, List(), List(),  StmtNil, StmtNil,clsP,methP)
    })
    Debug.prntDebugInfo("fake push handler", res1.length)
  //  val res2 = genLinked(List())(res1)
  //  res2 map (_.asInstanceOf[PushHandlerStmt])
    
    val res2 = CommonUtils.linkedListWrapper(List())(res1)
    
    res2 match {
      case Some(l) => {
        val linkedLstflt= CommonUtils.flattenLinkedStmt(List())(l)
         Debug.prntDebugInfo("linked push handler", linkedLstflt.length)
        Some(linkedLstflt)
      }
      case None => None
    }
    
  }

  def genPopHandlerStmts(catches: List[CatchStmt], clsP: String, methP: String): Option[List[Stmt]]= {
    val res1 = catches map ((c: CatchStmt) => PopHandlerStmt(c.exceptionType, StmtNil, StmtNil, clsP, methP))
   // val res2 = genLinked(List())(res1)
   // res2 map (_.asInstanceOf[PopHandlerStmt])

      val res2 = CommonUtils.linkedListWrapper(List())(res1.reverse)
    res2 match {
      case Some(l) => {
        val fltPopStmts = CommonUtils.flattenLinkedStmt(List())(l)
        Debug.prntDebugInfo("fltPopSmtts are: \n", fltPopStmts )
        Some(fltPopStmts)
      }
      case None => None
    }
  }

  def genLinked(res: List[Stmt])(lst: List[Stmt]): List[Stmt] = {
    lst match {
      case Nil => res
      case hd :: tl => {
        tl match {
          case Nil => {
            hd.next = StmtNil
            res ::: List(hd)
          }
          case hd2 :: rest => {
            hd.next = hd2
            genLinked(res ::: List(hd))(rest)
          }
        }
      }
    }
  }

  def transFormBody(bd: Stmt, exnHandler: ExceptionHandlers, annot: List[String], clsP:  String, methP: String): Stmt = {
    bd match {
      case StmtNil => StmtNil
      case _ => {
        var flattendList = CommonUtils.flattenLinkedStmt(List())(bd)
        Debug.prntDebugInfo("Before transforming the method, length", flattendList.length)
        var lblStmts = extractsLabelStmts(bd)
        Debug.prntDebugInfo("total label stmts: ", lblStmts.length)
        var catchStmts = extractCatchStmts(bd).reverse // fxi the order
        val startToCatches = bldStartingOrEndingLblCatches(catchStmts, true)
        val endToCatches = bldStartingOrEndingLblCatches(catchStmts, false)
        Debug.prntDebugInfo("startToCatches\n", startToCatches)
         Debug.prntDebugInfo("endToCatches\n", endToCatches)
         
        val startingkeys = startToCatches.keySet
        Debug.prntDebugInfo("Starting Labels", startingkeys)
        val endkeys = endToCatches.keySet
        Debug.prntDebugInfo("Ending Labels", endkeys)
        var res: Stmt = StmtNil

        for (stl <- startingkeys) {
          flattendList = CommonUtils.flattenLinkedStmt(List())(bd)
          Debug.prntDebugInfo("in for starting key", flattendList.length)
           Debug.prntDebugInfo("current label starting key", stl)
          val lblIndx = flattendList.indexOf(stl)
           Debug.prntDebugInfo("cur index", lblIndx)
          val nxtLblSt = flattendList(lblIndx + 1) 
          Debug.prntDebugInfo("next one", nxtLblSt)

          val catchHandlers = startToCatches get stl match {
            case Some(l) => l
          }
          Debug.prntDebugInfo("catch handler", catchHandlers)
          val pushHandStmtsOl = genPushHandlerStmts(catchHandlers, exnHandler, annot, clsP, methP)

          pushHandStmtsOl match {
            case Some(pushHandStmts) => {
                Debug.prntDebugInfo("lnked push handlers", pushHandStmts)
              stl.next = pushHandStmts.head
              Stmt.updateLabelWith(stl.label, stl)
              val oldLast = pushHandStmts.last
              val secondLast = pushHandStmts(pushHandStmts.indexOf(oldLast) -1) // because there is one more StmtNil generated as the last
              secondLast.next = nxtLblSt
              flattendList = CommonUtils.flattenLinkedStmt(List())(bd)
              Debug.prntDebugInfo("After inserting starting label", flattendList.length)
              //PushHandlerStmt(clsName: String, lbl: String, nxt: Stmt)
              res = bd
            }
            case None => res
          }
         
        }
        for (stl <- endkeys) {
          val catchHandlers = endToCatches get stl match {
            case Some(l) => l
          }
          Debug.prntDebugInfo("catch Handler length: ", catchHandlers)
          val pphtsol = genPopHandlerStmts(catchHandlers, clsP, methP)
          pphtsol match {
            case Some(pphts) => {
              val curLblInx = flattendList.indexOf(stl)
              val prevInx = curLblInx - 1
              val prvStmt = flattendList(prevInx)
             Debug.prntDebugInfo("After gen catch Handler length: " + stl, pphts)  
             
              // impossible for two label stmt sticks together,
              // so no need to change Stmt.labeltable.
              prvStmt.next = pphts.head
              val oldLast = pphts.last
              val secondLast = pphts(pphts.indexOf(oldLast) -1)
               Debug.prntDebugInfo("supposed to be a pop stmt " + stl + " " + oldLast, secondLast)
              secondLast.next = stl
                Debug.prntDebugInfo("secondlast pop's next ",secondLast.next)
              flattendList = CommonUtils.flattenLinkedStmt(List())(bd)
              Debug.prntDebugInfo("After inserting END label", flattendList.length)
              res = bd
            }
            case None => res
          }

        }
        
        res
      }
    }

  }

class ParseException (str: String) extends Exception

}