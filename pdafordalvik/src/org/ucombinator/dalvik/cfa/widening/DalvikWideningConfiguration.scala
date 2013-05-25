package org.ucombinator.dalvik.cfa.widening
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.dalvik.syntax.Stmt

trait DalvikWideningConfiguration extends StateSpace with WideningConfiguration{
 
  
   def widening(st: ControlState) : ControlState  = {
     // widening starts
     if(st.getCurFreq > wideningFreq) {
       
       val (curStore, curPs) = st.getCurWidenedStore
       val widenedStore = mergeTwoStores(curStore, st.getCurStore)
       val widenedPS = mergeTwoStores(curPs, st.getCurPropertyStore)
       wideningState(st, widenedStore, widenedPS)
     }else 
       st
   }
  
}