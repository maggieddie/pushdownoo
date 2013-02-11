package org.ucombinator.dalvik.specialAPIs
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.cfa.cesk.CESKMachinary
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.dalvik.syntax.{AExp, RegisterExp}
import org.ucombinator.utils.Debug
import org.ucombinator.utils.CommonUtils
import org.ucombinator.dalvik.vmrelated.APISpecs
import org.ucombinator.dalvik.exceptionhandling.ExceptionHandling
import org.ucombinator.dalvik.syntax.StForEqual
import org.ucombinator.dalvik.preanalysis.LiveRegisterAnalysis

trait ExternalLibCallsHandler extends StateSpace with CESKMachinary with  RawStringLibsAI with ReflectionAI with ExceptionHandling with LiveRegisterAnalysis{
  def isExternalLibCalls(cls:String) : Boolean = {
    CommonUtils.isStringLibs(cls) || CommonUtils.isMetaLibCall(cls) || APISpecs.isInAPISpecsbyName(cls)
  }
  
  def isOldSpecialAPI(meth: String) : Boolean = {
    CommonUtils.isStringLibs(meth) || CommonUtils.isMetaLibCall(meth)
  }
  
   def handleSpecialCalls(methPath:String, invokS: Stmt,argRegExps: List[AExp], objVals: Set[ObjectValue], ls: Stmt, s: Store, realN: Stmt, fp: FramePointer, kptr: KAddr, t: Time, tp: Time, k: Kont): Set[Conf] = {
    methPath match {
      case "java/lang/StringBuilder/<init>" =>  handleStringBuilderInit(invokS, argRegExps, objVals, ls, s, realN, fp, kptr, t, tp, k)
      case "java/lang/StringBuilder/append" => handleStringBuilderAppend(invokS, argRegExps, objVals, ls, s, realN, fp, kptr, t, tp, k)
      case "java/lang/String/valueOf" => handleStringValueof(invokS, argRegExps, objVals, ls, s, realN, fp, kptr, t, tp, k)
      
      //meta calls
      case "java/lang/Class/getName" => handleClassGetName(invokS, argRegExps, objVals, ls, s, realN, fp, kptr, t, tp, k)
      case "java/lang/Class/forName" => handleClassForName(invokS, argRegExps, objVals, ls, s, realN, fp, kptr, t, tp, k)
    }
   }
   
     def handleExternalLibCalls(methPath:String, invokS: Stmt,argRegExps: List[AExp], objVals: Set[ObjectValue], ls: Stmt, s: Store, realN: Stmt, fp: FramePointer, kptr: KAddr, t: Time, tp: Time, k: Kont): Set[Conf] = {
      val exnList = APISpecs.getAPIExns(methPath)
      val exnList2 = if(methPath == "java/lang/StringBuilder/append") {
        List()
      } else exnList
     
      
      /**
       * TODO: the clsP should be fixed
       */
      val liveRegs = Stmt.liveMap.getOrElse(buildStForEqual(realN), Set())
      val injStates = injectAPIFaultStates(exnList2, fp, s, k, t, invokS, "",  "", kptr, liveRegs)
      if(!isOldSpecialAPI(methPath)) {
        injStates ++ Set((PartialState(StForEqual(realN, realN.next, realN.clsPath, realN.methPath, realN.lineNumber), fp, s, kptr, tp), k))
      }else{
        injStates++ handleSpecialCalls(methPath, invokS,argRegExps, objVals, ls, s, realN, fp, kptr, t, tp, k)
      }
     }
   

}