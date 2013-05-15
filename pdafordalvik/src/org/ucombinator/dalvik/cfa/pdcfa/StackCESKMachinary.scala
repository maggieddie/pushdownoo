/**
 * @author shuying
 */

package org.ucombinator.dalvik.cfa.pdcfa

import org.ucombinator.dalvik.cfa.cesk._
import org.ucombinator.dalvik.syntax._
import org.ucombinator.utils.StringUtils
import org.ucombinator.utils.Debug
import org.ucombinator.utils.CommonUtils
import org.ucombinator.dalvik.specialAPIs.RawStringLibsAI
import org.ucombinator.dalvik.vmrelated.APISpecs
import org.ucombinator.dalvik.statistics.Statistics
import org.ucombinator.playhelpers.AnalysisHelperThread


trait StackCESKMachinary extends CESKMachinary with TransitionHandlers {

  //type Kont = List[Frame]

  // StackCESK machine has no continuation pointer
  type KAddr = Unit

  /**
   * **
   * XXX: TODO: to make it FLEXIBILY parameterized
   */
  def initState(s: Stmt, methP: String, store:Store, pstore: PropertyStore): Conf = (PartialState(buildStForEqual(s ), new FramePointer(List(), s // methP
  ), store, pstore, (), List()), Nil)

  /**
   * ************************************************
   *  Main non-deterministic abstract step function
   *  (it is so hard to abstract common logic of the
   *  step function into super type!
   * ***********************************************
   */
  //def mnext: Conf => Set[Conf] = {
  def mnext(conf: Conf): Set[Conf] = {
    conf match {

      /**
       * ******************
       * Core transitions
       * ******************
       */
      //goto
      case c @ (ps @ PartialState(st@StForEqual(gl @ (GotoStmt(lbl, nextSt, lineSt, clsP, metP)), nstt, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(st)
        
        Debug.prntDebugInfo("@In GotoStmt: ", gl)
        //  Debug.prntDebugInfo("belong to line: ", gl.ln.lineNumber)
        val curN = gl.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)

        val tp = tick(t, List(gl))
        val nextStOption = Stmt.forLabel(lbl)
        val nextSt =
          nextStOption match {
            case Some(nextst) => {
              nextst 
            }
            case None => throw new SemanticException("GotoStmt's label stmt unfound in Label Table!")
          }
        val realN1 = CommonUtils.findNextStmtNotLineOrLabel(nextSt)
        Set((PartialState(buildStForEqual(realN ), fp, s, pst, kptr, tp), k))
      }

      //nop
      case c @ (ps @ PartialState(st@StForEqual(nop @ (NopStmt(nextSt, lineSt, clsP, metP)), nstt, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
       updateHeatMap(st)
        
        Debug.prntDebugInfo("@In Nop: ", nop)

        val curN = nop.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)
        val tp = tick(t, List(nop))
        Set((PartialState(buildStForEqual(realN ), fp, s, pst, kptr, tp), k))
      }

      //if
      case c @ (ps @ PartialState(st@StForEqual(ifS @ IfStmt(cond, sucLabel, nxt, ls, clsP, metP), nstt, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(st)
        Debug.prntDebugInfo("@IfStmt: ", ifS)
        val curN = ifS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)

        //here we will explore the two branches.
        // so not cmopute the condition at all.
        val tp = tick(t, List(ifS))

        val nextStOption = Stmt.forLabel(sucLabel)
        val nextLblSt =
          nextStOption match {
            case Some(nextst) => nextst
            case None => throw new SemanticException("If sucess label stmt unfound in Label Table!" + ifS.toString())
          }
        val nextSt = CommonUtils.findNextStmtNotLineOrLabel(nextLblSt.next)
        Set((PartialState(buildStForEqual(nextSt ), fp, s, pst, kptr, tp), k),
          (PartialState(buildStForEqual(realN ), fp, s, pst, kptr, tp), k))
      }

      // packed-swtich or sparse-swtich
      /**
       * we will explore all the branches + the fall through state
       * no bother to test the register
       */
      case c @ (PartialState(st@StForEqual(pswS @ SwitchStmt(testReg, offset, labls, nxt, ls, clsP, metP), nxtt, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(st)
        Debug.prntDebugInfo("@@SwitchStmt: ", pswS)
        Debug.prntDebugInfo("belong to line: ", pswS.lineNumber)
        val curN = pswS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)
        val lblStrs = pswS.lables
        Debug.prntDebugInfo("stmt Map is ", Thread.currentThread().asInstanceOf[AnalysisHelperThread].stmtMap)
        handleSwitch(pswS, lblStrs, realN, fp, s, pst, kptr, t, k)
      }

      /**
       * **************
       * invoke; ah
       * 1. InvokeStmt: non-static, so no object register, but climing up the class ladders
       * 2. InvokeStaticStmt: no  climing up the class ladders
       * 3. invoke super
       * 4. invoke direct
       * 5. invoke interface
       */
      //InvokeStaticStmt(methPathStr: String, argRegAExp: List[AExp], tyStrs: List[String], nxt: Stmt)
      case c @ (ps @ PartialState(ste@StForEqual(ivkS @ InvokeStaticStmt(methPath, argRegExps, tyStrs, nxt, ls, clsP, metP), nxtt, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(ste)
        //("@InvokeStaticStmt: ", ivkS)
        Debug.prntDebugInfo("belong to line: ", ivkS.lineNumber)
        val curN = ivkS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        
        collectPerms(methPath)

        val tp = tick(t, List(ivkS))
        // yeah, for static method, it is class method. but we can extract classname from invoke static;)
        val clsName = StringUtils.getClassPathFromMethPath(methPath)
        Debug.prntDebugInfo(" before to get class Path", methPath)
        Debug.prntDebugInfo(" after the classname got ", clsName)

        if (isExternalLibCalls(methPath)) {
          val strRegExp = argRegExps.head
          val possibleValues = atomEval(strRegExp, fp, s)
          val objVals = filterObjValues(possibleValues)
          handleExternalLibCalls(methPath, ivkS, argRegExps, List(),  objVals, ls, s, pst, realN, fp: FramePointer, kptr, t, tp, k, ste)
        } /**
           * in the calling API with exceptions thrown, one brach is continue, normal case.
           * The other case is to branch to injected thrown statse.
           */ /*
          else
            if(APISpecs.isInAPISpecsbyName(methPath) ) {
            val exnsThrown = APISpecs.getAPIExns(methPath)
            val injStates = injectAPIFaultStates(exnsThrown, fp, s, k, t, ivkS, kptr)
            injStates ++ Set((PartialState(realN, fp, s, kptr, tp), k))
          } */ else {
          val resolvedMethds = DalvikClassDef.lookupMethod(clsName, methPath, tyStrs, 0)

          resolvedMethds match {
            case Nil => {

              Set((PartialState(buildStForEqual(realN ), fp, s, pst,kptr, tp), k))
            }
            case hd :: tl => {
              Debug.prntDebugInfo(" found method! ", hd.methPath)
              Debug.prntDebugInfo("  the parsed realN is invoke static is:", realN)
              // println("current found method:", hd.methodPath)
              /**
               * ToDO: the clasPth and ethPath
               */
              val nextLiveRegs =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].liveMap.getOrElse(buildStForEqual(realN), Set())
             // val injStates = getInjectStatesFromAnnotations(hd.localHandlers, hd.annotationExns, fp, s, k, t, ivkS, "", "", kptr, nextLiveRegs )
              //("start here?")
              applyMethod(ste, false, hd.body, hd.regsNum, None, fp, s, pst, k, List[AExp](), argRegExps, List(), t, ivkS, realN, kptr) //++ injStates
               
            }
          }
        }
      }

      case c @ (ps @ PartialState(ste@StForEqual(ivkS @ InvokeDirectStmt(methPath, argRegExps, objExp, tyStrs, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
       updateHeatMap(ste)
        val curN = ivkS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        collectPerms(methPath)
        
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        val tp = tick(t, List(ivkS))
        val objAexp = ivkS.objAExp
        handleNonStaticInvoke(
          "direct",
          ivkS,
          methPath,
          realN,
          objExp,
          objAexp,
          argRegExps,
          ls,
          fp,
          tyStrs, tp, s, pst, kptr, t, k, ste)
      }

      case c @ (ps @ PartialState(ste@StForEqual(ivkS @ InvokeInterfaceStmt(methPath, argRegExps, objExp, tyStrs, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
         collectPerms(methPath)
        updateHeatMap(ste)
        val curN = ivkS.next

        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        
        val tp = tick(t, List(ivkS))
        val objAexp = ivkS.objAExp
        handleNonStaticInvoke(
          "interface",
          ivkS,
          methPath,
          realN,
          objExp,
          objAexp,
          argRegExps,
          ls,
          fp,
          tyStrs, tp, s, pst, kptr, t, k, ste)
      }

      case c @ (ps @ PartialState(ste@StForEqual(ivkS @ InvokeStmt(methPath, argRegExps, objExp, tyStrs, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
    	  collectPerms(methPath)
        updateHeatMap(ste)
        
    	  val curN = ivkS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        //println("RealNext is: ", realN)

        val tp = tick(t, List(ivkS))
        val objAexp = ivkS.objAExp
        handleNonStaticInvoke(
          "virtual",
          ivkS,
          methPath,
          realN,     
          objExp,
          objAexp,
          argRegExps,
          ls,
          fp,
          tyStrs, tp, s, 
          pst, kptr, t, k, ste)

      }

      case c @ (ps @ PartialState(ste@StForEqual(ivkS @ InvokeSuperStmt(methPath, argRegExps, objExp, tyStrs, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
    	  collectPerms(methPath)
    	  updateHeatMap(ste)
        val curN = ivkS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        //println("RealNext is: ", realN)

        val tp = tick(t, List(ivkS))
        val objAexp = ivkS.objAExp
        handleNonStaticInvoke(
          "super",
          ivkS,
          methPath,
          realN,
          objExp,
          objAexp,
          argRegExps,
          ls,
          fp,
          tyStrs, tp, s, pst, kptr, t, k, ste)

      }
      //case class AssignAExpStmt(lhReg: AExp, rhExp: AExp, nxt: Stmt, ls : Stmt) extends Stmt {
      case c @ (ps @ PartialState(sfe@StForEqual(assignS @ AssignAExpStmt(lhReg, rhExp, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(sfe)
        Debug.prntDebugInfo("@AssignAExpStmt: ", assignS)
        Debug.prntDebugInfo("belong to line: ", assignS.lineNumber)
        val curN = assignS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)

        val tp = tick(t, List(assignS))

        handleAExpAssign(sfe, assignS, lhReg, rhExp, s, pst, realN, fp, kptr, tp, k)

      }

      case c @ (ps @ PartialState(sfq@StForEqual(newS @ NewStmt(destReg, clsName, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(sfq)
        Debug.prntDebugInfo("@NewStmt", newS)
        Debug.prntDebugInfo("belong to line: ", newS.lineNumber)
        val curN = newS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)
        val tp = tick(t, List(newS))
        val destRegExp = newS.destRegister
        val destAddr = fp.offset(newS.destReg.toString())
        if (isStringBulder(clsName)) {  
          val newOP = ObjectPointer(t, clsName, newS.lineNumber)
          val objVal = ObjectValue(newOP, clsName)
          // modified to do strong update
          val newStore = storeStrongUpdate(s, List((destAddr, Set(objVal))))
          	// the propertyStore should also be strongupdated
          val newPStore = propagatePStore(pst, clsName , sfq ,  List(destAddr ), true )  
         // val newPStore = storeStrongUpdate(pst)
          Set((PartialState(buildStForEqual(realN ), fp, newStore,  newPStore, 
              kptr, tp), k))
          //  Set((PartialState(realN, fp, s, kptr, tp), k))
        } else { 
          val newOP = ObjectPointer(t, clsName, newS.lineNumber)
          val objVal = ObjectValue(newOP, clsName)
          
          val newStore = storeUpdate(s, List((destAddr, Set(objVal))))
           val newStore2 = initObject(newS.classPath, newStore, newOP) 
           
           if(newS.sourceOrSink > 0) {
        	   // val objSecurityValue = SecurityValue(sfq.clsPath, sfq.methPath,  newS.lineNumber, newS.classPath, newS.sourceOrSink) 
        	   val objSecuVals = genTaintKindValueFromStmt(sfq.oldStyleSt)
        	   val newPStore = propagatePStore(pst, sfq.clsPath , sfq ,  List(destAddr ), false )  
        	   /**
        	    * The taint store here, currently we will progagate the object's property to the all its field 
        	    */ 
        	   val newPStore2 = initObjectProperty(newS.classPath, newPStore, newOP,objSecuVals) 
        	   val newState = (PartialState(buildStForEqual(realN ), fp, newStore2,  newPStore2, 
        			   kptr, tp), k)
        			   Set(newState)
           }
           else{ 
        	   val newState = (PartialState(buildStForEqual(realN ), fp, newStore2, pst, kptr, tp), k)
        	   Set(newState)
           }
        }
      }

      // case class FieldAssignStmt(lhr: AExp, fe: AExp, nxt: Stmt, ls: Stmt)  extends Stmt {
      case c @ (ps @ PartialState(st@StForEqual(fldAssgS @ FieldAssignStmt(lhReg, rhExp, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(st)
        Debug.prntDebugInfo("@FieldAssignStmt", fldAssgS)
        Debug.prntDebugInfo("belong to line: ", fldAssgS.lineNumber)
        val curN = fldAssgS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)
        val tp = tick(t, List(fldAssgS))

        handleFieldAssign(fldAssgS, s, pst, realN, fp, kptr, tp, k)

      }

      //case class MoveExceptionStmt(nameReg: AExp, nxt: Stmt, ls: Stmt) extends Stmt
      case c @ (ps @ PartialState(st@StForEqual(mvExpS @ MoveExceptionStmt(nameReg, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(st)
        Debug.prntDebugInfo("@MoveExceptionStmt", mvExpS)
        Debug.prntDebugInfo("belong to line: ", mvExpS.lineNumber)
        val curN = mvExpS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)
        val tp = tick(t, List(mvExpS))

        val objVals = storeLookup(s, fp.offset("exn"))
        val destRegExp = nameReg match {
          case RegisterExp(_) => nameReg.asInstanceOf[RegisterExp]
          case _ => throw new SemanticException("MoveExceptionStmt expects the nameReg to be RegisterExp, found: " + nameReg)
        }
        //println("move-exception: "+mvExpS, objVals.toList.length)
        val destAddr = fp.offset(destRegExp.regStr)
        val newStore = storeUpdate(s, List((destAddr, objVals)))
        
        val newState = (PartialState(buildStForEqual(curN ), fp, newStore, pst, kptr, tp), k)
        Set(newState)
      }

      //case class PopHandlerStmt(nxt: Stmt, ls: Stmt) extends Stmt {
      case c @ (ps @ PartialState(st@StForEqual(popHS @ PopHandlerStmt(exnT, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), hf @ HandleFrame(handlerType, clsName, lbl) :: k) => {
    	  updateHeatMap(st)
        val curN = popHS.next
        val curN2 = curN.next
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)

        val tp = tick(t, List(popHS))
        Set((PartialState(buildStForEqual(realN ), fp, s, pst, kptr, tp), hf))
        //Set((PartialState(buildStForEqual(realN ), fp, s, pst, kptr, tp), k)) // exception analysis rule
      }
      //case class PushHandlerStmt(clsName: String, lbl: String, nxt: Stmt, ls: Stmt) extends Stmt {
      case c @ (ps @ PartialState(st@StForEqual(pushHS @ PushHandlerStmt(typeStr, clsName, lbl, toFork, exnHandlers, exnAnnos, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(st)
        Debug.prntDebugInfo("@PushHandlerStmt", pushHS)
        Debug.prntDebugInfo("belong to line: ", pushHS.lineNumber)
        val curN = pushHS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        val tp = tick(t, List(pushHS))
        val pushHandlerFrame = new HandleFrame(pushHS.typeString, pushHS.className, pushHS.label)

        val normalState = Set((PartialState(buildStForEqual(realN ), fp, s, pst, kptr, tp), pushHandlerFrame :: k))

        Set((PartialState(buildStForEqual(realN ), fp, s, pst, kptr, tp),  k)) //doing nothing
      //  Set((PartialState(buildStForEqual(realN ), fp, s, pst, kptr, tp), pushHandlerFrame :: k)) // exception analysis rule
      }

      //case class ReturnStmt(resultAe: AExp, nxt: Stmt, ls: Stmt) extends Stmt {
      case c @ (ps @ PartialState(st@StForEqual(retS @ ReturnStmt(resultAe, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), FNKFrame(callerNxtSt, fpCaller) :: k) => {
        //("@ReturnStmt", retS)
        updateHeatMap(st)
        val curN = retS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)
        val tp = tick(t, List(retS))
        val realCallerNext = CommonUtils.findNextStmtNotLineOrLabel(callerNxtSt)
        resultAe match {
          case RegisterExp(sv) => {
            val resultRegAe = resultAe.asInstanceOf[RegisterExp]
            if (resultRegAe.regStr.isEmpty()) {
              Set((PartialState(buildStForEqual(realCallerNext ), fpCaller, s, pst, kptr, tp), k))
            } else { // return-* some register
              Debug.prntDebugInfo("@ReturnStmt: Return-*", retS)
              val retAddr = getReturnOffSet(fpCaller)
              val retVal = atomEval(resultAe, fp, s)
              val retRegStr = getRegExpStr(resultAe)
              val retPropertyVals = pStoreLookup(pst, fp.offset(retRegStr))
              
          //    val newStore = storeUpdate(s, List((retAddr, retVal)))
               val newStore = storeStrongUpdate(s, List((retAddr, retVal)))
               val newPStore = pStoreUpdate(pst, List((retAddr, retPropertyVals)))
              val newState = (PartialState(buildStForEqual(realCallerNext ), fpCaller, newStore,   newPStore,
                  kptr, tp), k)

              Set(newState)
            }
          }
          case _ => {
            throw new SemanticException("Return statement operator is not type RegisterExp" + c.toString())
          }
        }
      }

      // self looping control state with handleFramePopped
      case c @ (ps @ PartialState(st@StForEqual(retS @ ReturnStmt(resultAe, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), hf@HandleFrame(handlerType, clsName, lbl) :: k) => {
        // 	Debug.prntDebugInfo("@@ ReturnStmt with top frame is HandleFrame!: ", retS)
    	  updateHeatMap(st)
    	  Set((ps, hf)) 
    	  //Set((ps, k))   exception flow analysis rules
      }

      /**
       * exception flow analysis rules commented out for testing
       */
     /* case c @ (ps @ PartialState(stq@StForEqual(tS @ ThrowStmt(exn, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), Nil) => {
        updateHeatMap(stq)
        val tp = tick(List(tS), t)
        val exnRegExp = getRegExp(exn, "Throw Statement: Register Expression expected. Found: ")
        val exnVals = atomEval(exnRegExp, fp, s)
        val objVals = filterAbsObjValues(exnVals)
         //println("uncaught exanv vals: ", objVals.toList.length)
        val liveRegs =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].liveMap.getOrElse(buildStForEqual(nxss), Set())
       // println(lss + clsPP + methPP)
          Statistics.recordThrowPointsTo(stq, objVals.map(_.toString))
        thrownUncaughtExnStates(objVals, s, pst, fp, kptr, tp, clsP, metP, liveRegs)
      }

      case c @ (ps @ PartialState(stq@StForEqual(tS @ ThrowStmt(exn, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), FNKFrame(_, _) :: k) => {
       //println("ThrowStmt fnk on top")
        updateHeatMap(stq)
        val exnVals = atomEval(exn, fp, s)
        val objVals = filterAbsObjValues(exnVals)
       
         Statistics.recordThrowPointsTo(stq, objVals.map(_.toString))
        Set((ps, k))
      }

      //case class ThrowStmt(exn: AExp, nxt: Stmt, ls: Stmt) extends Stmt {
      case c @ (ps @ PartialState(stq@StForEqual(tS @ ThrowStmt(exn, nxt, ls, clsP, metP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), hf @ HandleFrame(handlerType, clsName, lbl) :: k) => {
    	  updateHeatMap(stq)
      // println("@ThrowStmt: ", tS)
        Debug.prntDebugInfo("belong to line: ", tS.lineNumber)
        val curN = tS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)
        val tp = tick(t, List(tS))
        val exnRegExp = getRegExp(exn, "Throw Statement: Register Expression expected. Found: ")
        val exnVals = atomEval(exnRegExp, fp, s)
        val objVals = filterAbsObjValues(exnVals)
      
        Statistics.recordThrowPointsTo(stq, objVals.map(_.toString))
        //handleNormalThrownStmtdef(tS, objVals.map(_.asInstanceOf[Value]), s, realN, fp, kptr, tp, k)
        handleNormalThrownStmt(ps, handlerType, clsName, lbl, tS, objVals.map(_.asInstanceOf[Value]), s, pst, nxt, fp, kptr, t, k)
      }*/

      /*   case c @ (ps @ PartialState(fis @ FaultInjectorStmt(exnHandlers, exnAnnos, nxt, ls), fp, s, kptr, t), k) => {

        getInjectFaultStates(exnHandlers, exnAnnos, fp, s, k, t, fis, kptr)

      }*/

     /* case c @ (ps @ PartialState(stq@StForEqual(itS @ InjectThrownStmt(exnValues, nxt, ls, clsP, methP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), FNKFrame(_, _) :: k) => {
    	updateHeatMap(stq)   
        Statistics.recordThrowPointsTo(stq, exnValues.map(_.toString))
        Set((ps, k))
      }

      case c @ (ps @ PartialState(stq@StForEqual(tS @ InjectThrownStmt(exnVals, nxt, ls, clsP, methP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), Nil) => {
       updateHeatMap(stq)
        val tp = tick(List(tS), t)
        val nextLiveRegs =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].liveMap.getOrElse(buildStForEqual(nxss), Set())
        thrownUncaughtExnStates(exnVals.map(_.asInstanceOf[AbstractObjectValue]), s, pst, fp, kptr, tp, clsP, methP, nextLiveRegs)
      }

      /**
       * For injected thrown state
       * according to the ls number, the state can be two cases:
       * case 1: if the
       */
      case c @ (ps @ PartialState(stq@StForEqual(itS @ InjectThrownStmt(exnValues, nxt, ls, clsP, methP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), hf @ HandleFrame(handlerType, clsName, lbl) :: k) => {
        updateHeatMap(stq)
        Statistics.recordThrowPointsTo(stq, exnValues.map(_.toString))
        handleNormalThrownStmt(ps, handlerType, clsName, lbl, itS, itS.exnValues, s, pst, nxt, fp, kptr, t, k)
        // handleInjectExnStmt(itS, s, nxt, fp, kptr, t, k)
      }*/

      case c @ (PartialState(stq@StForEqual(eS @ EntryPointInvokeStmt(en, objRegStr, nxt, ls, clsP, methP), nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
    	  stq
        // Debug.prntDebugInfo("belong to line: ", eS.lineNumber)
    	  updateHeatMap(stq)
        val curN = eS.next
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        val argTypeList = en.argTypes
        Debug.prntDebugInfo("@@EntryPointInvokeStmt: the argLength ", argTypeList.length)
        val methP = eS.entr.methodPath
        // gotta get the instance register
        val curObjAddrOffset = fp.offset(objRegStr)
        val possibleObjVals = storeLookup(s, curObjAddrOffset)

        val curObjVals = filterObjValues(possibleObjVals)

        Statistics.recordCallObjs(stq, curObjVals.map(_.toString))
        
        if (curObjVals.isEmpty) {
       
          throw new StackCESKException("the entry point invoke statmt can't find its instance object to invoke on!!!" + eS)
        } else {
          println("**************** Current EntryPointInvoke Statement is: ", eS)
          println("********* numbers of objects:", curObjVals.toList.length)
          curObjVals.map((curObjVal) => {
            val absValues = argTypeList.map(typeToTopValue(_, curObjVal.op))
           
            Debug.prntDebugInfo("@@EntryPointInvokeStmt: abs ", absValues.length)
            applyMethod(stq, true, en.body, en.regsNum, Some(curObjVal), fp, s, pst,  k, List(RegisterExp(SName.from(objRegStr))), List(), absValues, t, eS, realN, kptr)
          }).flatten
        }
      }

      /**
       * The stmt is added to initialize the entry point
       * after this, there will be multiple component entry points/handlers
       * after each init, if tehre were multiple init functions for a class
       * that the entry point belongs to
       */
      case c @ (PartialState(stq@StForEqual(ieS @ InitEntryPointStmt(methodPath, argsTypes, body, regsNum, nxt, ln, clsP, methP), nsxx, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        updateHeatMap(stq)
        Debug.prntDebugInfo("@@@InitEntryPointStmt: ", ieS)
        val curN = ieS.next
        Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        Debug.prntDebugInfo("RealNext is: ", realN)
        val thisRegStr = CommonUtils.getThisRegStr(regsNum, argsTypes.length)
        val thisRegExpOffset = fp.offset(thisRegStr)
        val methP = ieS.methodPath
        val entryClassName = StringUtils.getClassPathFromMethPath(methP)

        // create the initial ObjetValue for the class
        val newOP = ObjectPointer(t, entryClassName, ieS.lineNumber)
        val objVal = ObjectValue(newOP, entryClassName)

        // instantiate the class field map ... does strong update on the register help?
        val newStore = storeStrongUpdate(s, List((thisRegExpOffset, Set(objVal))))
        // initialize the fields of the currnet class and return new store?
        val newStore2 = initObject(entryClassName, newStore, newOP)

        val absValues = argsTypes.map(typeToTopValue(_, newOP))
        Debug.prntDebugInfo("New Object Created at entry init :" + ieS, (thisRegExpOffset, Set(objVal)))
        applyMethod(stq,true, body, regsNum, Some(objVal), fp, newStore2, pst, k, List(RegisterExp(SName.from(thisRegStr))), List(), absValues, t, ieS, realN, kptr)
      }
      //
      case c @ (ps @ PartialState(stq@StForEqual(StmtNil, nsxx, line, clsPP, methPP), fp, s, pst, kptr, t), Nil) => {
        updateHeatMap(stq)
        Debug.prntDebugInfo("@StmtNil: empty Nil ", "")
        Debug.prntDebugInfo("curstore is: ", s)
        Set((FinalState(), Nil))
      }
      case c @ (ps @ PartialState(stq@StForEqual(StmtNil, nsxx, line, clsPP, methPP), fp, s, pst, kptr, t), k) => {
       updateHeatMap(stq)
        Debug.prntDebugInfo("@StmtNil: The parital state has reached the StmtNil! but there are more kontinuations.", k.toList.length)
        // Debug.prntDebugInfo("Next is: ", nxt)
        Debug.prntDebugInfo("curstore is: ", s)
        Set((FinalState(), Nil))
      }
      //for unhandled instructions, move forward to the next stmt
      case c @ (ps @ PartialState(stq@StForEqual(stmt, nxss, lss, clsPP, methPP), fp, s, pst, kptr, t), k) => {
        //         println("@ unHandled!!!!!!: empty Nil K is "+ stmt + "\n", k)
        updateHeatMap(stq)
        val tp = tick(t, List(stmt))
        // Debug.prntDebugInfo("current is:  ", stmt)
        val curN = stmt.next
        // Debug.prntDebugInfo("CurNext is: ", curN)
        val realN = CommonUtils.findNextStmtNotLineOrLabel(curN)
        //  println("RealNext is: ", realN)
        Set((PartialState(buildStForEqual(realN ), fp, s, pst, kptr, tp), k))
      }
      /**
       * Alright, let's get out
       */
      case (FinalState(), Nil) => Set()

    }
  }

  class StackCESKException(str: String) extends Exception

}