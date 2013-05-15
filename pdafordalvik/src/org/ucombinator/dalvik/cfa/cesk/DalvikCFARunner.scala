package org.ucombinator.dalvik.cfa.cesk

import org.ucombinator.utils._
import org.apache.commons.lang3.StringEscapeUtils 

abstract class DalvikCFARunner(opts: AIOptions) extends AnalysisRunner(opts) with StateSpace with FancyOutput  {


  def prettyPrintState(state: ControlState, map: Map[ControlState, Int]): String = {
    val result: String = if (simplify) {
      map.get(state) match {
        case Some(n) => n.toString
        case None => {
          throw new Exception("Index not found for state " + state.toString)
        }
      }
    } else state match {
      case p@PartialState(s, fp, store, ps, kptr, t) => {
       map(state).toString()
      //  (StringUtils.truncateIfLong(s.toString, 100) ) //+
        /*  "\\n" + " CurFP = " + fp.meth +
          "\\n" + "  store hash = " + store.hashCode().toString +
          "\\n" + kptr.toString +
          "\\n" + t.toString)*/
      }
      case FinalState() => "Final(" + ")"
      case ErrorState(_, _) => "ErrorState"
    }
    StringEscapeUtils.escapeJava(result)
  }
  
    def prettyPrintState2(state: ControlState, map: Map[ControlState, Int]): String = {
    val result: String = if (simplify) {
      map.get(state) match {
        case Some(n) => n.toString
        case None => {
          throw new Exception("Index not found for state " + state.toString)
        }
      }
    } else state match {
      case p@PartialState(s, fp, store, ps, kptr, t) => {
         map(state).toString + "$" + 
         s.clsPath + "\\n" + s.methPath + "\\" + s.lineSt.toString
         
        
      }
      case FinalState() => "Final(" + ")"
      case ErrorState(_, _) => "ErrorState"
    }
    StringEscapeUtils.escapeJava(result)
  }
  
  private def prettyPrintStore(store: Store) : String = {
   StringEscapeUtils.escapeJava( 
    store.foldLeft("")((res, kv) => {
      val addr = kv._1
      val valSet = kv._2
     
      res +
      "* Addr:          " + addr + 
      "<br>" + "* Abstract Values: " + 
      "<br>" + "     " + valSet.foldLeft("")((res2, v) => res2 + v + "</br>")+ "<br>" 
    })
    )
  }
  
  def genPrettyStateToHtml(st: ControlState, map: Map[ControlState, Int]) : String = {
    val result = 
      st match {
      case p@PartialState(s, fp, store, pst, kptr, t) => {
        
         //println("empty valset?-----------" + pst.toString)
        "<b>Program Insruction and Context: </b>" +  
        (StringUtils.truncateIfLong(StringEscapeUtils.escapeJava(s.oldStyleSt.toString), 1000)) + 
        //(StringUtils.truncateIfLong(s.toString, 1000))+
          "<br></br>" + "<b>Current Frame Pointer is: </b>" +  (StringUtils.truncateIfLong(fp.meth.toString + fp.t, 1000)) +
          "<br></br>" + "<b> Store Details: </b>" + 
          "<br></br>" +
          prettyPrintStore(store) +
          "<br></br>" + "<b> Taint Store Details: </b>" + 
          "<br></br>" +
          prettyPrintStore(pst) +
          "<br></br>" + "<b> Time: </b>"+ 
           "<br></br>" + t.toString + 
          "<br></br>"
      }
      case FinalState() => "<br></br>"+ "Final(" + ")"
      case ErrorState(_, _) => "<br></br>" + "ErrorState"
    }
    StringEscapeUtils.escapeJava(result)
  }
  
  
  
  /**
   * TODO: compute statistics: singleton, points-to
   */
  
  /*def computeSingletons(states: Set[ControlState], exp: Exp): (Set[Var], Set[Var]) = {

    val goodStates: Set[ControlState] = states.filter({
      case PState(_, _, _, _) => true
      case _ => false
    })

    val allEnvs: Set[Env] = goodStates.map {
      case PState(_, rho, _, _) => rho
    }

    // all variables
    val allVars: Set[Var] = allEnvs.map(rho => rho.keys).flatten

    val varAddrMap: Var :-> Set[Addr] = Map.empty


    val globalVarAddrMap: Var :-> Set[Addr] =
      allEnvs.foldLeft(varAddrMap) {
        // for all environments
        case (vaMap, env) => {
          val augmentedEnv: Var :-> Set[Addr] =
            env.map(vAddrs => {
              val (v: Var, addr: Addr) = vAddrs
              // already found addresses for variable v
              val foundAddr: Set[Addr] = vaMap.getOrElse(v, Set())
              // addresses in this environments
              val withNewAddr: Set[Addr] = foundAddr ++ Set(addr)
              (v, withNewAddr)
            })

          // Combine old and new maps together
          vaMap ++ augmentedEnv
        }
      }
  
    // Now stores..
    val allStores: Set[Store] = goodStates.map {
      case PState(_, _, s, _) => s
    }


    // Mapping variables to values
    var varValMap: Var :-> Set[Val] = Map.empty

    for {
      s <- allStores
      (v, ax) <- globalVarAddrMap
    } {
      val newValues: Set[Val] = ax.map(addr => s.getOrElse(addr, Set())).flatten[Val]
      val oldValues: Set[Val] = varValMap.getOrElse(v, Set())
      val newVarValMap: Var :-> Set[Val] = varValMap + ((v, newValues ++ oldValues))
      varValMap = newVarValMap
    }


    val singletonVars: Set[Var] = varValMap.filter {
      case (v, ax) => ax.size == 1
    }.toSet[(Var, Set[Val])].map {
      case (v, ax) => v
    }

    // (vars-total, vars-singletons)
    (allVars, singletonVars)

  }*/
}