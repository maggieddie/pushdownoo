package org.ucombinator.dalvik.cfa.widening
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.cfa.cesk.StmtForEqual

trait WideningHelperTrait extends StateSpace with StmtForEqual{

  import org.ucombinator.domains.CommonAbstractDomains.Store
   type Kont = List[Frame]
    // StackCESK machine has no continuation pointer
  type KAddr = Unit

  type Time = List[Stmt]

   def k : Int  =1
  def tick(currentTime: List[Stmt], oldTime: Time): List[Stmt] = {
    val nlst = currentTime ++ oldTime
    nlst.take(k)
  }
   
   def initState(s: Stmt, methP: String, store:Store, pstore: PropertyStore): Conf = (PartialState(buildStForEqual(s ), new FramePointer(List(), s // methP
  ), store, pstore, (), List()), Nil)

   
}