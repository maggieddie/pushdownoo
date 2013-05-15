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
import org.ucombinator.playhelpers.AnalysisHelperThread

trait ExternalLibCallsHandler extends StateSpace with CESKMachinary with  RawStringLibsAI with ReflectionAI with ExceptionHandling with LiveRegisterAnalysis{
  def isExternalLibCalls(cls:String) : Boolean = {
    CommonUtils.isStringLibs(cls) || CommonUtils.isMetaLibCall(cls) || APISpecs.isInAPISpecsbyName(cls)
  }
  
  def isOldSpecialAPI(meth: String) : Boolean = {
    CommonUtils.isStringLibs(meth) || CommonUtils.isMetaLibCall(meth)
  }
  
   def handleSpecialCalls(methPath:String, invokS: Stmt,argRegExps: List[AExp], objVals: Set[ObjectValue], ls: Stmt, s: Store, pst: PropertyStore, realN: Stmt, fp: FramePointer, kptr: KAddr, t: Time, tp: Time, k: Kont): Set[Conf] = {
    methPath match {
      case "java/lang/StringBuilder/<init>" =>  handleStringBuilderInit(invokS, argRegExps, objVals, ls, s, pst, realN, fp, kptr, t, tp, k)
      case "java/lang/StringBuilder/append" => handleStringBuilderAppend(invokS, argRegExps, objVals, ls, s, pst, realN, fp, kptr, t, tp, k)
      case "java/lang/String/valueOf" => handleStringValueof(invokS, argRegExps, objVals, ls, s, pst, realN, fp, kptr, t, tp, k)
      
      //meta calls
      case "java/lang/Class/getName" => handleClassGetName(invokS, argRegExps, objVals, ls, s, pst, realN, fp, kptr, t, tp, k)
      case "java/lang/Class/forName" => handleClassForName(invokS, argRegExps, objVals, ls, s, pst, realN, fp, kptr, t, tp, k)
    }
   }
   
  
   /**
    * 1. If the statement is not annotated to be sink or source, but the argument has anything like that, we will update in the objectstore
    * 2. of course, if the statement itself is annotated to be source or sink, we can do that.
    * 3. The other thing to do is to taint the move-result's register
    * 
    * what about we will join all the values from the arguments then then extends the stores of the head argeuemetn???
   
    */
    def propagateTaintPropertyForFunctionInvokeObjAndMoveResult(
    												argAddrs: List[Addr], 
		   											objAexps: List[AExp],
		   											pst: PropertyStore, 
		   											invokS: Stmt, 
		   											nextStmt: Stmt, 
		   											clsP: String, 
		   											methPath: String,
		   											lineNo:Stmt,
		   											fp: FramePointer  
		   											) : PropertyStore = {
     
     val (isMoveResultS, targetRegister) = isMoveResult(nextStmt)
     
     val argPropertyValues = argAddrs.flatMap((argAddr) => {
       pStoreLookup(pst, argAddr)
     }).toSet
   
      
    
     val anySensitiveVals = srcOrSinksSecurityValues(argPropertyValues)
     
     if(anySensitiveVals.isEmpty)  
       pst
    else {
       if (objAexps.isEmpty) { // static invoke, no need to affect the obejct, because no object be affected wtih
       
         if(isMoveResultS) { // the next is move result, pass the joined properties to the returned result
         //  val newPStore = pStoreUpdate(pst, List((fp.offset(targetRegister.toString), argPropertyValues)))
            val newPStore = pStoreUpdate(pst, List((fp.offset("ret"), argPropertyValues)))
            println("re assigned is", fp.offset("ret"))
           newPStore
         }
         else  pst
     }
       else {// normal invoke {
         // also next moveResult
        // this gonna propagating to the first argument, which is the object, as well the returning object
          val hdAddr =  fp.offset(objAexps.head.toString)
         if(isMoveResultS){
          
           //val newPStore = pStoreUpdate(pst, List((hdAddr, argPropertyValues)))
            val newPStore2 = pStoreUpdate(pst, //newPStore, 
                List((fp.offset("ret"), argPropertyValues)))
           newPStore2 
         }
         else{ // will only affect the object security values then
           
          // val newPStore = pStoreUpdate(pst, List((hdAddr, argPropertyValues)))
          // newPStore
           pst
         } 
    } 
    } 
   }
   
   
   
     def handleExternalLibCalls(methPath:String, 
    		 					invokS: Stmt,
    		 					argRegExps: List[AExp], 
    		 					objAexps: List[AExp], //static invoke will be empty
    		 					objVals: Set[ObjectValue], 
    		 					ls: Stmt, 
    		 					s: Store, 
    		 					pst: PropertyStore, 
    		 					realN: Stmt, 
    		 					fp: FramePointer, 
    		 					kptr: KAddr, 
    		 					t: Time, 
    		 					tp: Time, 
    		 					k: Kont,
    		 					stForEqual: StForEqual): Set[Conf] = {
       
      
              
       val argRegStrs = argRegExps map (getRegExpStr)
        val argRegAddrs = argRegStrs.map(fp.offset(_))
              
      val exnList = APISpecs.getAPIExns(methPath)
      val exnList2 = if(methPath == "java/lang/StringBuilder/append") {
        List()
      } else exnList 
      
      /**
       * TODO: the clsP should be fixed
       */
      val liveRegs =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].liveMap.getOrElse(buildStForEqual(realN), Set())
      val injStates = injectAPIFaultStates(exnList2, fp, s, pst, k, t, invokS, "",  "", kptr, liveRegs)
      
      
       val pst2 = propagateTaintPropertyForFunctionInvokeObjAndMoveResult(argRegAddrs, objAexps, pst , invokS , realN , stForEqual.clsPath,  stForEqual.methPath, stForEqual.lineSt,  fp )
       
      if(!isOldSpecialAPI(methPath)) {   
       
        val nextStp = Set((PartialState(StForEqual(realN, realN.next, realN.clsPath, realN.methPath, realN.lineNumber), fp, s,  pst2, 
            kptr, tp), k))
        injStates ++ nextStp 
       
          } 
      else{
        	//injStates ++  //exception analysis rules
        	handleSpecialCalls(methPath, invokS,argRegExps, objVals, ls, s,   pst2,
        	    realN, fp, kptr, t, tp, k)
          }
      }
    

}