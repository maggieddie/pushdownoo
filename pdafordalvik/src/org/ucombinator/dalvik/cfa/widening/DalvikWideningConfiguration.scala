package org.ucombinator.dalvik.cfa.widening
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.dalvik.syntax.Stmt

trait DalvikWideningConfiguration extends StateSpace with 
WideningConfiguration{
 
  import org.ucombinator.domains.CommonAbstractDomains._
   def widening(st: ControlState) : ControlState  = {
     // widening starts
     if(st.getCurFreq > wideningFreq) {
       
       val (curWStore, curWPs) = st.getCurWidenedStore
       val widenedStore = curWStore.join(st.getCurStore) //mergeTwoStores(curWStore, st.getCurStore)
       val widenedPS = curWPs.join(st.getCurPropertyStore) //mergeTwoStores(curWPs, st.getCurPropertyStore)
       wideningState(st, widenedStore, widenedPS)
     }else 
       st
   }
   
   
  
}