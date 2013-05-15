package org.ucombinator.dalvik.cfa.gc
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.utils.Debug
import org.ucombinator.dalvik.syntax.Stmt

trait DalvikGarbageCollector extends StateSpace with GarbageCollectorTrait {

  /**
   * The main GC interface
   */
  def gc(c:ControlState, frames: Kont) : ControlState = c match {
    case ErrorState(_, _) | FinalState() => c
    case PartialState(st, fp, store, ps, kptr,  t) => {
      val livingAddrs = reachable(c, frames)
      
      
      val cleanStore = store.filter {
        case (a, _) => livingAddrs.contains(a)
      }
      
      val cleanPStore = ps.filter {
         case (a, _) => livingAddrs.contains(a)
      }
      PartialState(st, fp, cleanStore, cleanPStore, kptr, t)
    }
  }
  
  def reachable(c: ControlState, frames : Kont) : Set [Addr] = {
    val rootAddresses: Set[Addr] = getRootAddrs(c, frames)
   
    
    c match{
      case ErrorState(_, _) | FinalState() => Set.empty
      case PartialState(st, fp, store, ps, kprt, t) => {
        val res : Set[Addr] = collectAdjacentAddrs(rootAddresses, store)
          Debug.prntDebugInfo("reachable : ", res)
        
        if(printGCDebug) {
          val before = store.keys.toSet
          val delta = before -- res
          if(! delta.isEmpty) {
            Debug.prntDebugInfo("Before GC, the store size is", before.size)
            Debug.prntDebugInfo("After GC, the size is: ", res.size)
           // println("Store delta size is: ", delta.size)
           // println("Detail Details: \n") 
            // delta.foreach(println)
          }
        }
        res
      }
    }
  }
  
  /**
   *  get addresses from a stack
   *  In Dalvik PDCFA, we onlt need to exam one FNKFrame
   */

  def getRootAddrsFromStack(f:Frame, s: Store) = f match {
    case FNKFrame(st, fp) => getAddrsofCurFP(fp, s)
    case _ => Set()
  }
  
  /**
   * Yes, I embed the logic to reach the live variable in this func
   * because currnetly we only deal with register, not field liveness
   */
 def getAddrsofCurFP(fp: FramePointer, s: Store) : Set[Addr] = {
   
   s.foldLeft(Set[Addr]())((res : Set[Addr], kv) => kv._1 match {
     // case class RegAddr(fp: FramePointer, offs: String) e
     case RegAddr(ifp, offs) => {
       ifp match {
         case dfp@FramePointer(_, _) => {
           //val fpFP = fp.asInstanceOf[FramePointer]
           if (ifp == fp) {
             
                 res + kv._1
             /**
              * if we do live range analysis 
              * and we have to reach only the live registers
              */
             /*if(doLRA) {
               val stOfFp = dfp.meth
               val liveRegStrs = Stmt.liveMap(stOfFp)
               if(liveRegStrs.contains(offs)) {
                 res + kv._1
               }
               // not live register,we don't collect it
               else res
             }
             // 
             else res + kv._1*/
           }
           else res
         }
         case _ => res
       }
     }
     case _ => res
   })
 }
 
 def AddrsofCurOP(op: ObjectPointer, s: Store) : Set[Addr] ={
  s.foldLeft(Set[Addr]())((res : Set[Addr], kv) => kv._1 match {
     //case class FieldAddr(op: ObjectPointer, field: String) extends OffsetAddr {
     case FieldAddr(iop, field) => {
       iop match {
         case ObjectPointer(_, _,_) => {
           val oop = iop.asInstanceOf[ObjectPointer]
           if (op == oop) {
               res + kv._1
           }else res
         }
         case _ => res
       }
     }
     case _ => res
   })
 }
  
 /**
  * reach to all the object field address along with register addr
  */
  def collectAdjacentAddrs (prevAddrs: Set[Addr], s: Store) : Set[Addr] ={
    val filterStore = s.filter {
      case (a, vals) => prevAddrs.contains(a)
    }
    
    Debug.prntDebugInfo("Before Store is" + s, "the filter store is"+ filterStore)
    
    val filterFlattenedVals = filterStore.flatMap {
      case (a, vals) => vals
    }
    
    val reachedObjVals = filterFlattenedVals.filter {
        case ObjectValue(op, clsName) => true
        case _ => false
    }.toSet
    
    val reachedOps = reachedObjVals.map {
      (v: Value) => v match {
        case ObjectValue(op, clsName) => op 
      }
    }
    
    val newAddrs : Set[Addr] = reachedOps.flatMap {
      op => AddrsofCurOP(op, s)
    }
    
    if(newAddrs subsetOf prevAddrs) {
      Debug.prntDebugInfo("the newAddrs are" + newAddrs, "the previous addrs are" + prevAddrs)
      prevAddrs
    } else{
      collectAdjacentAddrs(newAddrs ++ prevAddrs, s)
    }
  }
  
  
  
  
}