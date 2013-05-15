package org.ucombinator.dalvik.exceptionhandling
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.cfa.cesk.CESKMachinary
import org.ucombinator.utils.Debug
import org.ucombinator.utils.CommonUtils
import org.ucombinator.dalvik.syntax.{Stmt,StmtNil,DalvikClassDef, MethodDef, MoveExceptionStmt,RegisterExp}
import org.ucombinator.dalvik.syntax.ExceptionHandlers
import org.ucombinator.dalvik.syntax.ThrowStmt
import org.ucombinator.dalvik.cfa.cesk.StmtForEqual
import org.ucombinator.dalvik.preanalysis.LiveRegisterAnalysis
import org.ucombinator.dalvik.statistics.Statistics


trait ExceptionHandling extends StateSpace with CESKMachinary with StmtForEqual with LiveRegisterAnalysis{

   type Kont = List[Frame]
  
  type Time = List[Stmt]

  def tick(currentTime: List[Stmt], oldTime: Time): List[Stmt] = {
    val nlst = currentTime ++ oldTime
    nlst.take(k)
  }
   
  /**********
   * to inject faults
   **********/
  case class InjectThrownStmt(exnValues: Set[Value], nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
 	 var next = nxt
 	 var lineNumber = ls
  
 	 var clsPath = clsP
 	 var methPath = methP
 	 
 	 override def toString =  "InjectThrownStmt(" + exnValues + ")"
 	 
 	  def refRegsStrSet : Set[String] = {
 			 Set( )
  }
  
   def defRegsStrSet: Set[String] ={
    Set("exn")
   }
   
   def sourceOrSink = 0
    def taintKind =  Set[String]()
  }
  
  case class UncaughtExceptionStmt(uncaughtType: String, nxt: Stmt, ls: Stmt, clsP: String, methP: String) extends Stmt {
    var next = nxt
 	 var lineNumber = ls
  var clsPath = clsP
  var methPath = methP
 	 override def toString =  "UncaughtExceptionStmt: " + uncaughtType + " " 
 	 
 	  def refRegsStrSet : Set[String] = {
    		Set()
  }
  
   def defRegsStrSet: Set[String] ={
    Set()
   }
     def sourceOrSink = 0
      def taintKind =  Set[String]()
  }
  
  
  
  def injectFaultStates (
      injThrownStmts:List[InjectThrownStmt], 
      fp: FramePointer, 
      s: Store, 
      pst: PropertyStore,
      kptr: KAddr, 
      t:Time, 
      st: Stmt,
      k: Kont ) : Set[Conf] ={
    val tp = tick(List(st), t)
  injThrownStmts.foldLeft(Set[Conf]())((res, ist) => {
    res ++ Set((PartialState(buildStForEqual(ist ), fp, s, pst, kptr,tp), k))
  })   
 }
 
  /**
   * This is used to inject exception states for the method that uses throws
   */
  
   def getInjectStatesFromAnnotations(
     // methDef: MethodDef, 
       handlerMgr: ExceptionHandlers,
       annotationExnVals: List[String],
      fp: FramePointer, 
      s: Store, 
      pst:PropertyStore,
      k: Kont, 
      t: Time, 
      st: Stmt, 
         clsP: String, methP: String,
      kptr: KAddr,
      nextLiveRegs: Set[String]): Set[Conf] = {
    
    // val handlerMgr = methDef.localHandlers
   // val annotationExnVals = methDef.annotationExns
   
    /**
     * But there are also the exceptions that can be thrown by the annotations, 
     * but the handlers can not catch, we also branch to those states
     * 
     * here, the injected annoation state is just like throw statement,
     */
   // val tp = tick(List(st), t)
    
    val injectRestTypeStrs: Set[String] = annotationExnVals.toSet //-- handlerMgr.getAllHandlerTypes.toSet
    //println("injected annotations value", injectRestTypeStrs.toList.length)
   val genSts= genFaultStatesFromExnTypes(injectRestTypeStrs.toList, fp, s, pst, k, t, st, clsP, methP, kptr, nextLiveRegs)
  // println("the generated annotation states are of length: ", genSts.toList.length)
   genSts
   /* val injStmtLsts = injectRestTypeStrs.foldLeft(List[InjectThrownStmt]())((res, inTyStr) => {
       val linNO = CommonUtils.getThrownLineNumer
       val exnOp = ObjectPointer(t, inTyStr, st.lineNumber) // XX: NOTE: supposed we always can refernce to the line number here!
       val objVal = ObjectValue(exnOp, inTyStr)
       // so you see, the newly generated injstmt, the next stmt is StmtNil
       // but we are going to test on the lineNO later in the state with InjectThrownStmt
       val injStmt = InjectThrownStmt(Set(objVal), StmtNil, linNO)
       res ::: List(injStmt)
    })
    
    injectFaultStates(injStmtLsts, fp, s, kptr, t, k)*/
  }
  
  private def getHandlerBranchStates(
     // methDef: MethodDef, 
         handlerMgr: ExceptionHandlers,
       exnAnnos: List[String],
      fp: FramePointer, 
      s: Store, 
      pst: PropertyStore,
      k: Kont, 
      t: Time, 
      st: Stmt, 
      kptr: KAddr): Set[Conf] = {
    
    //yeah, we are going to brach to hte handler if there are any
   // val handlerMgr = methDef.localHandlers
    
    val tyLblPairs = handlerMgr.getNonFinallyExnAndNxtStPairs
    
    val injStmtLsts = tyLblPairs.foldLeft(List[InjectThrownStmt]())((res, p) => {
      /**
       * TODO fix the claPath and methPath
       */
      val clsP = ""
      val methP = ""
      val linNO = CommonUtils.getThrownLineNumer(clsP, methP)
       val exnOp = ObjectPointer(t, p._1, st.lineNumber) // XX: NOTE: supposed we always can refernce to the line number here!
       val objVal = ObjectValue(exnOp, p._1)
       // so you see, the newly generated injstmt, the next stmt is StmtNil
       val injStmt = InjectThrownStmt(Set(objVal), p._2, linNO, clsP, methP)
       res ::: List(injStmt)
    })
    injectFaultStates(injStmtLsts, fp, s, pst, kptr, t, st,k)
  }
 
  //private  
  def getInjectFaultStates( 
       exnHandlers: ExceptionHandlers,
       exnAnnos: List[String],
      // methDef: MethodDef, 
       fp: FramePointer, 
       s: Store, 
       pst: PropertyStore,
       k: Kont,
       t: Time, 
       st: Stmt, 
          clsP: String, methP: String,
       kptr: KAddr, liveRegs: Set[String]): Set[Conf] = {
    //val tp = tick(List(st), t)
    val restInjSts = getInjectStatesFromAnnotations(exnHandlers, exnAnnos, fp, s, pst, k, t, st, clsP, methP,kptr, liveRegs)
    val handlerBranchSts = getHandlerBranchStates(exnHandlers, exnAnnos, fp, s, pst, k, t, st, kptr)
    restInjSts ++ handlerBranchSts
  }
   
   private def  genFaultStatesFromExnTypes(
       exnTypes: List[String],  
       fp: FramePointer, 
       s: Store, 
       pst:PropertyStore,
       k: Kont,
       t: Time, 
       st: Stmt, 
       clsP: String, methP: String,
       kptr: KAddr,
       nextLiveRegs: Set[String]): Set[Conf] = {
    val injStmtLsts = exnTypes.foldLeft(List[InjectThrownStmt]())((res, inTyStr) => {
       val linNO = StmtNil //CommonUtils.getThrownLineNumer You mother fucker!!! line!!!!!!!!!!
       val exnOp = ObjectPointer(t, inTyStr, st.lineNumber) // XX: NOTE: supposed we always can refernce to the line number here!
       val objVal = ObjectValue(exnOp, inTyStr)
       // so you see, the newly generated injstmt, the next stmt is StmtNil
       // but we are going to test on the lineNO later in the state with InjectThrownStmt
       val injStmt = InjectThrownStmt(Set(objVal), StmtNil, linNO, clsP, methP)
      
       res ::: List(injStmt)
    })
     /**
        * Here when instrumenting the exception thrown state, we are doing something
        *  We'll need to extend it to the liveMap anyway, no matter the option is turned on or not  
        *  and in the way taht don't change current pre-computed fixed point!
        */
   if(injStmtLsts.isEmpty){
     
   }else {
     injStmtLsts.foreach((injS) => {
       val eqStIn = buildStForEqual(injS)
      Stmt.extendLiveMap(Map(eqStIn -> nextLiveRegs))
     })
    
    
   }
    
    injectFaultStates(injStmtLsts, fp, s,  pst,kptr, t, st, k)
   }
   
   def injectAPIFaultStates(
       exnTypes: List[String],
       fp: FramePointer, 
       s: Store, 
       pst: PropertyStore,
       k: Kont,
       t: Time, 
       st: Stmt, 
          clsP: String, methP: String,
       kptr: KAddr, liveRegs: Set[String]): Set[Conf] = {
     // val tp = tick(List(st), t)
      genFaultStatesFromExnTypes(exnTypes, fp, s,pst,  k, t, st, clsP, methP, kptr, liveRegs)
   }
     
  def thrownUncaughtExnStates(objVals: Set[AbstractObjectValue], s: Store, pst: PropertyStore, fp: FramePointer, kptr: KAddr, t: Time ,clsP: String, methP: String, liveRegs:Set[String]): Set[Conf] = {
     objVals.foldLeft(Set[Conf]())((res, objV)=>{
           val clsName = classTypeOfAbsObject(objV)
           val uc=UncaughtExceptionStmt(clsName, StmtNil, StmtNil, clsP, methP)
           /**
            * extend the livemap anyway
            */
           Stmt.extendLiveMap(Map(buildStForEqual(uc)->liveRegs))
           
          res ++ Set((PartialState(buildStForEqual(uc ), fp, s, pst, kptr, t), Nil))
        })
  }
   
   def handleNormalThrownStmt(ps: PartialState, handlerType:String, handleExnType:String, lbl:String, ts: Stmt, objValues: Set[Value], s: Store, pst: PropertyStore, nxt: Stmt, fp: FramePointer, kptr: KAddr, t: Time, k: Kont): Set[Conf] = {

     
    // dealWithAllPossibleExnValues(ts, objValues, s, fp, kptr, t, k)
      objValues.foldLeft(Set[Conf]())((res, absV) => {
         val clsName = classTypeOfAbsObject(absV)
      res ++ //dealWithOnePossibleExnValue(ts, absV, s, fp, kptr, t, k,objValues)
      handleHandleFrame(ps, handlerType, handleExnType, lbl, clsName ,  Set(absV), 
     k, fp, s, pst, kptr, t, ts) 
    })
   }
   
   /**
   * Deal with hte inject Stmt
   */
  
   def handleInjectExnStmt(itS: InjectThrownStmt, s: Store, pst: PropertyStore, nxt: Stmt, fp: FramePointer, kptr: KAddr, t: Time, k: Kont, clsP: String, methP: String): Set[Conf] = {
     val tp = tick(List(itS), t) 
     val lineS = itS.lineNumber
         /**
          * This is state injected by the annotation
          * The logic for this state is like ThrowStmt --> will peel away the stack. 
          * if found any handler, then branches to it; otherwise, uncaught state will be forked
          */
         if(CommonUtils.isAnnoThrownLineStmt(lineS)) {
           dealWithAllPossibleExnValues(itS, itS.exnValues.map(_.asInstanceOf[AbstractObjectValue]), s, pst, fp, kptr, t, k, clsP, methP)
         }
         /**
          * This is used to branch into all the catch handlers.
          */
         else {
           val exnVals = itS.exnValues
           val nxtSt = itS.next
           
           // we'll directly execute the move-exception
           nxtSt match {
             case mvS@MoveExceptionStmt(nameReg, nxt, ls, clsP, metP) => {
               val destAddr = nameReg match{
                 case re@RegisterExp(_) => {
                   val regStr = re.regStr
                   fp.offset(regStr)
                 }
                 case _ =>{
                   throw new SemanticException("the register in move-exceptionStmt is not RegisterExp " + nameReg)
                 }
               }
               val newNext = CommonUtils.findNextStmtNotLineOrLabel(mvS.next)
               //println("jumping to the caytch handler, after the move-exception, the newNext is ", newNext)
               val newStore = storeUpdate(s, List((destAddr,exnVals )))
               Set((PartialState(buildStForEqual(newNext ), fp, newStore, pst, kptr, tp), k))
               
             }
             // this must come from normal throw's change to InjectThrown!
             case _ => throw new CESKException("injecttExnStmt's next is not move Eception ")// Set((PartialState(itS, fp, s, kptr, tp), k))
           }
         }
   }

     
  
  
  /**
   * To deal one Exception Value
   */
  private def dealWithOnePossibleExnValue(ijt: Stmt, exnValue: Value, s: Store, pst: PropertyStore, fp: FramePointer, kptr: KAddr, t: Time, k: Kont, allAbsVals:Set[Value], clsP:String, methP: String): Set[Conf] = {
    val clsName = classTypeOfAbsObject(exnValue)
    handleThrown(clsName, Set(exnValue), k, fp, s, pst, kptr, t, ijt, allAbsVals,clsP, methP )
  }
  
 
  
   def dealWithAllPossibleExnValues(ijt: Stmt, allExnVals: Set[Value], s: Store, pst:PropertyStore, fp: FramePointer, kptr: KAddr, t: Time, k: Kont, clsP: String, methP: String): Set[Conf] = {
   
     allExnVals.foldLeft(Set[Conf]())((res, absV) => {
      res ++ dealWithOnePossibleExnValue(ijt, absV, s, pst, fp, kptr, t, k,allExnVals, clsP, methP)
    })
  }
  
  /**
   * This logic can be used both for Normal Thronw state and Injected thrown state
   * 
   * The throw, will peel of the stack. 
   * along the way, it will execute all the finally handlers
   * until it finds the catch handlers to deal with. 
   * 
   * When handler found, it will branch to that catch handler to execute, 
   * abandom all the rest statemnt of previous execution
   * 
   * When handler not found until the last frame (is the function kontinuation frame),
   * it will generate the uncaught exception state, which is also exiting abruptly
   * 
   * Oh, along the way, it also pops out all the function kontinuation. if it is not found 
   *        
   */
 private def handleThrown(exnType: String,  exnValue: Set[Value], k: Kont, fp: FramePointer, s: Store, pst:PropertyStore,  kptr: KAddr, t: Time, ijt: Stmt, allVals:Set[Value], clsP: String, methP: String) : Set[Conf] = {
   val tp = tick(List(ijt), t)
   k match {
     // There should be the one last function kontinuation on the stack we popped out all the handleframe.
     // in the case of uncaught exception
     case Nil => {
       //hd match {
      //   case fnk@FNKFrame(st, fp) => {
       val uc = UncaughtExceptionStmt(exnType, StmtNil, StmtNil,clsP, methP)
           Set((PartialState(buildStForEqual(uc ), fp, s, pst, kptr, tp), Nil))
         //}
         /* case hf@HandleFrame(handlerType, handleExnType, lbl) => {
            println("sees the handler frame!!!", k)
           Set((PartialState(UncaughtExceptionStmt(exnType, StmtNil, StmtNil), fp, s, kptr, tp), Nil))
         }*/
       
     }
     // this is the normal case. There are several posibility
     case  hd :: tl => {
      
       hd match {
         case hf@HandleFrame(handlerType, handleExnType, lbl) => {
           handlerType match {
             // we will fork to the finally along the way
             case "finally" => {
               val finallyHandlerStmtO = Stmt.forLabel(lbl)
               finallyHandlerStmtO match {
                 case Some(st) => {
                   val realN = CommonUtils.findNextStmtNotLineOrLabel(st)
                   val nxtN = getProperStmt(ijt, allVals)
                    val newStore = storeUpdate(s, List((fp.offset("exn"), exnValue)))
                  // One is the original injectthtown , the toher state is the finally
                //   Set((PartialState(nxtN, fp, s, kptr, tp), tl)) ++ 
                   Set((PartialState(buildStForEqual(realN ), fp, newStore, pst, kptr, tp), tl)) // shoudl the finally get the rest kontinuation.I think so
                 }
                 case None => {throw new CESKException("the finally handler is not inthe label table in injectThrown statement inspectStack")}
               }
               
             }
             // the normal case, the interpretation will just branches to it. if the type matches.
             case "normal" => {
              
               // if found the exacty type or super type exception handler
               if((exnType == handleExnType) || DalvikClassDef.isInstanceofParents(exnType,handleExnType)){
               
                  val handlerStO = Stmt.forLabel(lbl)
                   handlerStO match {
                   case Some(st) => {
                      val realN = CommonUtils.findNextStmtNotLineOrLabel(st)
                      val newStore = storeUpdate(s, List((fp.offset("exn"), exnValue)))
                      Set((PartialState(buildStForEqual(realN), fp, newStore, pst, kptr, tp), tl))
                   }
                   case None =>  {throw new CESKException("the finally handler is not inthe label table in injectThrown statement inspectStack")}         
                 }
               }
               // we continue to pop based on the InjectThrownStmt, based on hte current rest frames{
               
               else{
               
                   val nxtN = getProperStmt(ijt, allVals)
                    Set((PartialState(buildStForEqual(nxtN ) , fp, s, pst, kptr, tp), tl))
               }
             }
           }
         }
         case fnk@FNKFrame(st, fp)  => { //just conitnue to pop.
           val nxtS = getProperStmt(ijt, allVals)
          // println(nxtS)
           Set((PartialState(buildStForEqual(nxtS ) , fp, s, pst, kptr, tp), tl))
         }
         
         
       } //end hd :: tl
     }
       //stop  if other kind of kontinuation happens!!!
       case _ => {
         throw new CESKException("The Kontinuation has excceptainal state!" + k)
       }
     }
 }
 
 def handleHandleFrame(ps: PartialState, handlerType:String, handleExnType:String, lbl:String, exnType: String,  exnValue: Set[Value], 
     k: Kont, fp: FramePointer, s: Store, pst: PropertyStore, kptr: KAddr, t: Time, ijt: Stmt) : Set[Conf] = {
     val tp = tick(List(ijt), t)
    //  println(" gonna catch the type: ", exnType)
   handlerType match {
             case "finally" => {
                 //("in the finally handle, the exception Type: ", exnType)
               val finallyHandlerStmtO = Stmt.forLabel(lbl)
               finallyHandlerStmtO match {
                 case Some(st) => {
                   val realN = CommonUtils.findNextStmtNotLineOrLabel(st)
                    Statistics.recordEC(buildStForEqual(ijt),buildStForEqual(realN ) )
                   val nxtN =ijt //getProperStmt(ijt, allVals)
                    val newStore = storeUpdate(s, List((fp.offset("exn"), exnValue)))
                  // One is the original injectthtown , the toher state is the finally
                 //  Set((PartialState(nxtN, fp, s, kptr, tp), k)) ++ 
                   Set((PartialState(buildStForEqual(realN ), fp, newStore, pst, kptr, tp), k)) // shoudl the finally get the rest kontinuation.I think so
                 }
                 case None => {throw new CESKException("the finally handler is not inthe label table in injectThrown statement inspectStack")}
               }
               
             }
             // the normal case, the interpretation will just branches to it. if the type matches.
             case "normal" => {
              //println("in the normal handle, the exception Type: ", exnType)
               // if found the exacty type or super type exception handler
               if((exnType == handleExnType) || DalvikClassDef.isInstanceofParents(exnType,handleExnType)){
              //   println("find handler: ", handleExnType)
              
                  val handlerStO = Stmt.forLabel(lbl)
                   handlerStO match {
                   case Some(st) => {
                      val realN = CommonUtils.findNextStmtNotLineOrLabel(st)
                      
                      Statistics.recordEC(buildStForEqual(ijt),buildStForEqual(realN ) )
                      
                      val newStore = storeUpdate(s, List((fp.offset("exn"), exnValue)))
                      Set((PartialState(buildStForEqual(realN ), fp, newStore, pst, kptr, tp), k))
                   }
                   case None =>  {throw new CESKException("the finally handler is not inthe label table in injectThrown statement inspectStack")}         
                 }
               }
               else{ //  if not found, it will continue to loop!
                   val nxtN = ijt//getProperStmt(ijt, allVals)
                    //Set((PartialState(buildStForEqual(nxtN ), fp, s, kptr, tp), k))
                   //println("")
                   Set((ps, k))
               }
             }
           }
   
 }
 
 private def getProperStmt(curSt: Stmt, vals:Set[Value]) : Stmt = {
   curSt match {
     case ijs@InjectThrownStmt(vals, _, _,_,_) => {
       ijs
     }
     case ts@ThrowStmt(_, _, _,_,_) => {
       curSt //InjectThrownStmt(vals, ts.next,CommonUtils.genNewThrownLineNumber)
     }
   }
 }
 
 
 
 

 private def classTypeOfAbsObject(aob: Value) : String = {
    aob match{
              case objv@ObjectValue(_,_) => objv.className
              case objT@ObjectSomeTop(_) => objT.className
            }
  }
 
 
 /**
   *  called in throw transitions.
   *  we first filter the handleframe in the kontinuation
   * 1.  then we get handle frames of the exact same type of the throw type
   * 2.  we also get handle frames of the super type of the thrown type
   *  if 1 is not empty, we will transfer control flow to that label in the handle fmra, we only grab one since it is the usual case.
   *  if 1 is empty, we see whether 2 is empty or not
   *  if 2 is not empty, we return to that frames. Usually, there might be several frames there
   *  if 1 and 2 fails, we move to the next statement - the exception is not handled (locally or globally)
   *
   *  Yes, there is the merging here due to the object types, because the exception objects
   *  
   *  TO BE DELTED
   */
  def handle(objValues: Set[ObjectValue], s: Store, pst: PropertyStore, nxt: Stmt, fp: FramePointer, kptr: KAddr, tp: Time, k: Kont): Set[Conf] = {

    val curHandleFrames = filterHandleFrames(k)
    objValues.flatMap((objV: ObjectValue) => {
      val clsName = classTypeOfAbsObject(objV)
      val sameTypeFrames = framesOfSpecifiedType(curHandleFrames, clsName, true)
      val superTypeFrames = framesOfSpecifiedType(curHandleFrames, clsName, false)

      if (!sameTypeFrames.isEmpty) {
        Debug.prntDebugInfo("Same exception type" + clsName + " number: ", sameTypeFrames.length)
        val newStore = storeUpdate(s, List((fp.offset("exn"), Set(objV))))
        Debug.prntDebugInfo("@updated store: ", newStore)
        // It is expected to be one,so we grab one
        val nextSO = Stmt.forLabel(sameTypeFrames.head.handlerLabel)
        val actualNS = nextSO match {
          case Some(nextS) => CommonUtils.findNextStmtNotLineOrLabel(nextS)
          case None => throw new SemanticException("handle label not found in Label Table!" + sameTypeFrames.head.handlerLabel)
        }
        val newState = (PartialState(buildStForEqual(actualNS ), fp, newStore, pst, kptr, tp), k)
        Set(newState)

      } else if (!superTypeFrames.isEmpty) {
        Debug.prntDebugInfo("Super exception type" + clsName + " number: ", superTypeFrames.length)
        val newStore = storeUpdate(s, List((fp.offset("exn"), Set(objV))))
        Debug.prntDebugInfo("@updated store: ", newStore)
        // It is expected to be one,so we grab one
        val nextSO = Stmt.forLabel(superTypeFrames.head.handlerLabel)
        val actualNS = nextSO match {
          case Some(nextS) => CommonUtils.findNextStmtNotLineOrLabel(nextS)
          case None => throw new SemanticException("handle label not found in Label Table!" + sameTypeFrames.head.handlerLabel)
        }
        val newState = (PartialState(buildStForEqual(actualNS ), fp, newStore, pst, kptr, tp), k)
        Set(newState)
      } else {
        // if we don't find any exception type matched, 
        val nextSO = Stmt.forLabel(superTypeFrames.head.handlerLabel)
        val actualNS = CommonUtils.findNextStmtNotLineOrLabel(nxt)
        Set((PartialState(buildStForEqual(actualNS ),fp, s, pst, kptr, tp), k))
      }
    })
  }

  private def filterHandleFrames(k: Kont): List[HandleFrame] = {
    val hfs =
      k.filter(fr => fr match {
        case HandleFrame(_, _, _) => true
        case _ => false
      })

    hfs map (_.asInstanceOf[HandleFrame])
  }

  private def framesOfSpecifiedType(allFrames: List[HandleFrame], thrownType: String, isExact: Boolean): List[HandleFrame] = {
    if (isExact) {
      allFrames.filter((f: HandleFrame) => {
        if (f.exceptionClasspath == thrownType) true
        else false
      })
    } else {
      allFrames.filter((f: HandleFrame) => {
        if (DalvikClassDef.isInstanceofParents(thrownType, f.exceptionClasspath)) true
        else false
      })
    }
  }
}