package org.ucombinator.dalvik.statistics
import scala.collection.immutable.{ Set => ImmSet, Map => ImmMap}
import org.ucombinator.dalvik.syntax.StForEqual

object Statistics {


  // E-C Links
  var  ecTable: ImmMap[StForEqual, Set[StForEqual]] = ImmMap()
  
  def recordEC(exnThrownSt: StForEqual, handlerStmt: StForEqual) {
    val oldHandler  = ecTable.getOrElse(exnThrownSt, Set())
    ecTable += (exnThrownSt -> (Set(handlerStmt) ++ oldHandler) )
  }
  
  def totalAndAverageEclinks: (Int, Double) = {
      def tblFilter(a: Any) : Boolean = true
    val cnt = ecTable.count(tblFilter)
     val totalcardi =  ecTable.foldLeft(0)((sum, keyValue: (StForEqual, Set[StForEqual])) => {
       val (k, vs) = keyValue
       val cardi = vs.toList.length
       sum + cardi})
       
       val average = if(cnt == 0) 0 else  (totalcardi.toDouble / cnt).toDouble 
      (totalcardi,  Math.ceil(average)) 
  }
  
  // Throw Points to
  
  var throwPointsToTbl: ImmMap[StForEqual,  Set[String]] = ImmMap()
 
  
  def recordThrowPointsTo (throwSt: StForEqual, exnObjs: Set[String]) {
     val oldObjs  = throwPointsToTbl.getOrElse(throwSt, Set())
      throwPointsToTbl += (throwSt ->  (exnObjs ++ oldObjs) )
  }
  
  def totalAndMeanThrowPointsTo: (Int, Double) = {
     def tblFilter(a: Any) : Boolean = true
    val cnt = throwPointsToTbl.count(tblFilter)
     val totalcardi =  throwPointsToTbl.foldLeft(0)((sum, keyValue: (StForEqual,  Set[String])) => {
       val (k, vs) = keyValue
       sum + vs.toList.length})
       
       val average = if(cnt == 0) 0 else  (totalcardi.toDouble / cnt).toDouble 
      (totalcardi,  Math.ceil(average)) 
  }
  
  //callSite Points to
   var callObjsTbl: ImmMap[StForEqual, Set[String]] = ImmMap()
  
  def recordCallObjs (throwSt: StForEqual, exnObjNum: Set[String]) {
     val oldObjs  = callObjsTbl.getOrElse(throwSt, Set())
      callObjsTbl += (throwSt ->  (exnObjNum ++ oldObjs) )
  }
  
  def totalAndMeanCallObjs: (Int, Double) = {
     def tblFilter(a: Any) : Boolean = true
    val cnt = callObjsTbl.count(tblFilter)
     val totalcardi =  callObjsTbl.foldLeft(0)((sum, keyValue: (StForEqual, Set[String])) => {
       val (k, vs) = keyValue
       sum + vs.toList.length })
       
       val average = if(cnt == 0) 0 else  (totalcardi.toDouble / cnt).toDouble 
      (totalcardi,  Math.ceil(average)) 
  }

}