package org.ucombinator.dalvik.statistics
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.syntax.{Stmt, ThrowStmt}
import org.ucombinator.dalvik.exceptionhandling.ExceptionHandling
import org.ucombinator.dalvik.syntax.StForEqual

/**
 * @author shuying
 *
 */

trait DalvikAnalysisStatistics extends StateSpace with ExceptionHandling{
  /**
 * Large bulk of the code is about computing the points to information
 *
 */
  case class VarPointsTo(totalEntries: Int, totalCardi: Int)
  case class ThrowPointsTo(totalEntries: Int, totalCardi: Int)
  //case class ECLinks(totalEntries: Int, totalCardi: Int)

  case class AnalysisStatistics(
    analysisTime: Long,
    varPointsTo: VarPointsTo,
    throwPointsTo: ThrowPointsTo,
    numStates: Int,
    numEdges: Int,
    truncated: Boolean)
   
  
  /* private def filterRegisterStates (states: Set[ControlState]): Set[ControlState] = {
     states.filter({
      case PartialState(_, _, _, _,_) => true
      case _ => false
    })
   }*/
  
  private def filterThrownStates (states: Set[ControlState]) : Set[ControlState] ={
    states.filter ({
      case PartialState ( StForEqual(stmt, _, _,_,_ ),  _, _,_,_,_ ) => {
        isThrownSt(stmt)
      }
      case _ => false
    })
  }
  

  
  def isThrownSt(st: Stmt) : Boolean = {
    st match {
      case  ThrowStmt(_, _, _,_,_) => true
      case  InjectThrownStmt(_,_,_,_,_) => true
      case _ => false
    }
  }
  
  private def getPointsToElemPair(monoStore: Store) : ( Int, Int) = {
      def storeFilter(a: Any) : Boolean = true
      val totalcardi =  monoStore.foldLeft(0)((sum, keyValue: (Addr, Set[Value])) => {
       val (k, vs) = keyValue
       val cardi = vs.toList.length
       sum + cardi})
      (monoStore.count(storeFilter), totalcardi)
  }
   
  
  def computePointsToStatistics (states: Set[ControlState]) : (VarPointsTo, ThrowPointsTo) = {
    val regularStates = filterRegisterStates(states)
    val thrownStates = filterThrownStates(states)
    
    //println("thrown states: ", thrownStates.toList.length)
    
    val regularMonoStore = getMonovariantStore(regularStates)
    val throwMonStore = getMonovariantStore(thrownStates)
    
    val (totalReguEntries, totalReguCardi) = getPointsToElemPair(regularMonoStore)
    val (totalThrownEntries,  totalThrownCardi) = getPointsToElemPair(throwMonStore)
   
    //println(totalReguEntries) 
    //println(totalReguCardi)
    (VarPointsTo(totalReguEntries, totalReguCardi), 
     ThrowPointsTo(totalThrownEntries, totalThrownCardi))
  }
  
}




