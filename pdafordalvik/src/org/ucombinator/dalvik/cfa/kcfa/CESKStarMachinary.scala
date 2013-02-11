/**
 * Meant to be implemented for comparison. 
 * Now give up for a while! F.
 */

/*package org.ucombinator.dalvik.cfa.kcfa
import org.ucombinator.dalvik.cfa.cesk.CESKMachinary
import org.ucombinator.dalvik.cfa.cesk.StmtForEqual
import org.ucombinator.dalvik.syntax.{Stmt}

trait CESKStarMachinary extends CESKMachinary with StmtForEqual{

   type Time = List[Stmt]

   // new time
  def tick(currentTime: List[Stmt], oldTime: Time): List[Stmt] = {
    val nlst = currentTime ++ oldTime
    nlst.take(k)
  }
   
  *//**
   * Definition
   *//*
   type Kont =   List[Frame]  // don't need that.but need to define anyway
   
   *//********************************************************************
   * Continuations with pointers
   ********************************************************************//*
  abstract sealed class PointerKont

  case class Pointed(frame: Frame, kptr: KAddr) extends PointerKont 

  object MT extends PointerKont

  
  *//**
   * Utilities
   *//*
    
  // oh yes, the MF
   def initState(s: Stmt, methP: String, store:Store, pstore: PropertyStore): Conf = {
     (PartialState(buildStForEqual(s ), new FramePointer(List(), s ), store, pstore, (), List()), Nil)
   }
     
  
  def mnext(conf: Conf): Set[Conf] = {
    Set()
  }
  
  
  *//**
   * Evaluate the CESK* machine
   *//*
  def evaluateKCFA(e: Exp): (Set[Edge], Set[Conf]) = {
    val initialStates = Set(initState(e))
    iterateKCFA(initialStates, Set(), initialStates)
  }
  
  
  
   *//**
   * Kleene iteration of a work set of states
   *//*
  private def iterateKCFA(workSet: Set[Conf], edges: Set[Edge], accumStates: Set[Conf]): (Set[(Conf, Conf)], Set[Conf]) = {

     val newConfsEdges: Set[(Conf, Edge)] = workSet.map((c: Conf) => {
      val next: Set[Conf] = mnext(c)
      val cleanNext = if (shouldGC) {
        next.map {
          case (c1@PState(_, _, _, kaddr), kont) => {
            (gc(c1, kont), gcKStore(kaddr, kont))
          }
          case q => q
        }
      } else {
        next
      }
      cleanNext.map(x => (x, (c, x)))
    }).flatten

    val (newStates, newEdges) = newConfsEdges.unzip

    println(progressPrefix + " " + accumStates.size + " states computed so far.")

    val collectedEdges : Set[Edge] = edges ++ newEdges

    if (newStates.subsetOf(accumStates)) {
      (collectedEdges, accumStates)
    } else if (interrupt && accumStates.size > interruptAfter) {
      (collectedEdges, accumStates)
    } else {
      iterateKCFA(newStates, collectedEdges, accumStates ++ newStates)
    }
  }

  type Edge = (Conf, Conf)

  
  
}*/