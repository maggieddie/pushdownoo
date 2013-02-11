/**
 * @author shuying
 * 
 * This implementation has done a lot optimization based on the IPDCFA paper (Ilya's imlp):
 * 	1. Olin's aggresive cut off - based on subsumption testing
 * 	2. The original algorithm blindly add successors to todo set, which is optimized away 
 * 	3. Of course, the godel hashing scheme really helps!
 *
 */


package org.ucombinator.dsg
import org.ucombinator.dalvik.cfa.gc.GarbageCollectorTrait
import org.ucombinator.utils.FancyOutput
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.utils.Debug
import org.ucombinator.utils.{ StringUtils, AIOptions, FancyOutput }
import tools.nsc.io.Directory
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.dalvik.cfa.widening.WideningConfiguration
import org.ucombinator.dalvik.syntax.PopHandlerStmt
import org.ucombinator.perfmeasure.Measure
import org.ucombinator.domains.CommonAbstractDomains

trait DyckStateGraphMachinery extends StateSpace {
  self: GarbageCollectorTrait with FancyOutput with WideningConfiguration =>

  import org.ucombinator.domains.CommonAbstractDomains._
  /**
   * Abstract componnents
   */

  // type Frame
  //  type ControlState

  type EntryExp // term
  type AbsValue
  //  type Addr

  // type SharedStore = Map[Addr, Set[AbsValue]]
  type SharedStore = Store //Addr :-> Set[Value]
  type PSharedStore = Store // Addr :-> Set[Value]
  // def initState(e: Stmt, methP: String): (ControlState, Kont)

  def step(q: ControlState, k: Kont, frames: Kont, store: SharedStore, pStore: PSharedStore): Set[(StackAction[Frame], ControlState, SharedStore, PSharedStore)]

  // the following four, not sure about the real uage
  def mustHaveOnlyEmptyContinuation(s: ControlState): Boolean

  def canHaveEmptyContinuation(s: ControlState): Boolean

  def canHaveSwitchFrames: Boolean

  def isStoreSensitive(s: ControlState): Boolean

  /**
   *  Dyck State graph Nodes
   */
  type S = ControlState
  type Nodes = Set[S]
  type Kont = List[Frame]

  /**
   * Dyck State graph Edges
   */
  sealed case class Edge(source: S, g: StackAction[Frame], target: S)

  type Edges = Set[Edge]

  /**
   * DSG = (S,E,s0) -- a Dyck State graph
   * implicitly parametrized with ? (a set of frames)
   * s0 \in S -- initial node
   */
  sealed case class DSG(nodes: Set[S], edges: Edges, s0: S)

  private def noEdgesExplored: Int = {
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges
  }

  private def timePassed(cutTime: Long) = {
    cutTime - Thread.currentThread().asInstanceOf[AnalysisHelperThread].curThreadStartTime
  }

  private def noStatesExplored: Int = {
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfStates
  }

  private def incNoEdges(explored: Int) {
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges = Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfEdges + explored
  }

  private def incNoStates(explored: Int) {
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].noOfStates += explored
  }

  def evaluateDSG(e: Stmt, methP: String, store: CommonAbstractDomains.Store, pStore: CommonAbstractDomains.Store, firstHelper: NewDSGHelper): (DSG, CommonAbstractDomains.Store, CommonAbstractDomains.Store, String) = {
    import org.ucombinator.domains.CommonAbstractDomains._
    val initial = initState(e, methP, store, pStore)
    val initS = initial._1
    //compute the LFP recursively: trap
    def eval(next: DSG, helper: NewDSGHelper, shouldProceed: Boolean, statesToVisit: Set[S], store: SharedStore, pStore: PSharedStore): (DSG, NewDSGHelper, SharedStore, PSharedStore, String) = {

      val curTime = (new java.util.Date()).getTime

      // System.out.println("\n The DSG graph: ")
      // dumpDSGGraph2(next)
      if (!shouldProceed) {
        (next, helper, store, pStore, "during")
      } else if ((interrupt && (noEdgesExplored > interruptAfter || next.edges.size > interruptAfter)) ||
        (timeInterrupt && timePassed(curTime) > interruptAfterTime)) { //next.edges.size > interruptAfter){
        //println("no of edges explored: " + noEdgesExplored)
        (next, helper, store, pStore, "stop") //ce
      } else {
        val (next2, helper2, goAgain, newToVisit, newStore, newPStore) = iterateDSG(next, helper, statesToVisit, store, pStore)

        eval(next2, helper2, goAgain, newToVisit, newStore, newPStore)
      }
    }

    val firstDSG = DSG(Set(initS), Set(), initS)
    /**
     * Come on, we should using one helper for all the entry points!
     */
    // val firstHelper = new NewDSGHelper
    val (nextDSG, nextHelper, hasNew, toVisit, firstStore, firstPStore) = iterateDSG(firstDSG, firstHelper, Set(initS), store, pStore) //Map.empty)

    val (resultDSG, _, newStore, newPStore, toStopEntryExploration) = eval(nextDSG, nextHelper, hasNew, toVisit, firstStore, firstPStore)

    (resultDSG, newStore, newPStore, toStopEntryExploration)
  }

  def unzip4Components(res: Set[(S, Edge, SharedStore, PSharedStore)]): (Set[S], Set[Edge], Set[SharedStore], Set[PSharedStore]) = {
    var states = Set[S]()
    var edges = Set[Edge]()
    var sharedStore = Set[SharedStore]()
    var pSharedStore = Set[PSharedStore]()

    res.foreach((e) => {
      val (s, edge, ss, pss) = e
      states = states ++ Set(s)
      edges = edges ++ Set(edge)
      sharedStore = sharedStore ++ Set(ss)
      pSharedStore = pSharedStore ++ Set(pss)
    })

    (states, edges, sharedStore, pSharedStore)

  }

  // precond: weakerStates is subset of targetSet of possible Edges
  private def filterWeakerEdges(weakerStates: Set[ControlState], possibleEdges: Edges): Edges = {
    possibleEdges.filter(pe => {
      weakerStates.contains(pe.target)
    })
  }

  private def filterOutPopHandlerStates(states: Set[ControlState]): Set[ControlState] = {
    states.filter(spv => {
      spv.isPopHandlerState
    })
  }

  private def filterOutEdgesWithTargetInSet(edges: Edges, states: Set[ControlState]): Edges = {
    edges.filter(pe => {
      states.contains(pe.target)
    })
  }

  /*
  * Tuning the states!
  * @params: 
  * statesPossibletoVisit : successor statesl; 
  * possibleEdges: successor edges
  * epsNextStates: the folded set of next epsilon states
  * subsumption: based on subsumption
  */

  private def getStatesIfAco(helper: NewDSGHelper, statesPossibletoVisit: Set[ControlState], possibleEdges: Edges, epsNextStates: Set[ControlState], subsumption: Boolean): (Set[ControlState], Edges) = {

    /**
     * filter out the pophandlers control states before cut off
     * later will be added in
     */
    // filter out the pophandler states
    val pophandlerStates = filterOutPopHandlerStates(statesPossibletoVisit)

    //println("possible before filter: " + possibleEdges)
   //  println("PopHandlers:", pophandlerStates.size)
     //pophandlerStates.foreach(pps => println(pps.getCurSt))

    val statesPossibletoVisit2 = statesPossibletoVisit //statesPossibletoVisit -- pophandlerStates

    // filter the edges that end with the pophandler
    val popHandlerEdges = filterOutEdgesWithTargetInSet(possibleEdges, pophandlerStates)
    val possibleEdges2 = possibleEdges // -- popHandlerEdges

    // filter out the pophandler states statesPossibletoVisit
    val popEpsNextStates = filterOutPopHandlerStates(epsNextStates)
    //println("pophandlers from next: ")
    // popEpsNextStates.foreach(pps => println(pps.getCurSt))

    val epsNextStates2 = epsNextStates //-- popEpsNextStates

    if (aggresiveCutOff) {
      //filter out weaker ones
      val ss1 = statesPossibletoVisit2.filter((spv) => {
        val curStateEqualentStates = helper.getEpsPredStates(spv)
        !spv.weakerThanAny(curStateEqualentStates, subsumption) // false means equality
      })

      /**
       * Between branches, which is proved to be in the equivalence class w.r.t stack net chance
       * we cut off weaker states (refer to the partial order of control state)
       * I think this is very interesting
       */
      val sss1 = getStrongerStatesWithin(ss1, subsumption)

      val possibleNewEdges = possibleEdges2.filter(pe => {
        sss1.contains(pe.target)
      })

     // println("in aco: possibleEdges2 = possibleNewedges ", possibleEdges.size)
      //println("in aco: after filter out, larger: ", possibleNewEdges.size)
      val ss2 = epsNextStates2.filter((spv) => {
        val curStateEqualentStates = helper.getEpsPredStates(spv)
        !spv.weakerThanAny(curStateEqualentStates, subsumption)
      })

      /**
       * for epsilon next sucessors, also perform the filter
       */
      val sss2 = getStrongerStatesWithin(ss2, subsumption)

      println(statesPossibletoVisit.toList.length + "/" + ss1.toList.length + "/" + sss1.toList.length)
      println(epsNextStates.toList.length + "/" + ss2.toList.length + "/" + sss2.toList.length)

      val res1 = sss1 ++ sss2  // ++  popEpsNextStates ++  pophandlerStates 
      // println("After cutoff")
      // filterOutPopHandlerStates(res1).foreach(pps => println(pps.getCurSt))
      val res2 = possibleNewEdges  //++ popHandlerEdges
      (res1, res2)

    } else {
      (statesPossibletoVisit ++ epsNextStates, possibleEdges2 //++ popHandlerEdges
          )
    }
  }

  private def getStrongerStatesWithin(allSuccessors: Set[ControlState], subsumption: Boolean): Set[ControlState] = {

    val weakerStates = allSuccessors.filter(as => {
      // not considering itself.
      val restSs = allSuccessors - as
      as.weakerThanAny(restSs, subsumption)
    })

    allSuccessors -- weakerStates

  }

  private def decideNewNodesEdgesToVisit(newStates: Set[ControlState], ss: Set[ControlState], newEdges: Edges, subsumption: Boolean): (Set[ControlState], Edges) = {
   // println("--before aco ----" + newStates.size + " " + newEdges.size)
    if (aggresiveCutOff) {
      val weakerStates = getWeakerStates(newStates, ss, subsumption)
      val weakerEdges = filterWeakerEdges(weakerStates, newEdges)
      val newNewStates = newStates -- weakerStates // we are not going to explore weaker states 
      val newNewEdges = newEdges -- weakerEdges
     // println("--after aco ----" + newNewStates.size + " " + newNewEdges.size)
      (newNewStates, newNewEdges)
    } else {
      (newStates, newEdges)
    }
  }

  
  private def printOutStates (newToVisit : Set[S]) {
     val newToVisitStatements = newToVisit.map(vt => {
        vt.getCurSt
      })
      
     // println("has duplicated states?")
      newToVisitStatements.foreach(println)
  }
  /**
   * Monotonic DSG iteration function
   * denoted as 'f' in the paper
   */
  private def iterateDSG(dsg: DSG, helper: NewDSGHelper, toVisit: Set[S], store: SharedStore, pStore: PSharedStore): (DSG, NewDSGHelper, Boolean, Set[S], SharedStore, PSharedStore) = dsg match {
    case DSG(ss, ee, s0) => {

      // ECG contains all the previously explored states. 
      // and so, let's test the membership in the preds and nexts for each of the newly created states  
      val newNodesEdgesStores: Set[(S, Edge, SharedStore, PSharedStore)] =
        // Measure.measureTime("new states and stores:::") {
        for {
          s <- toVisit
          kont <- helper.getRequiredKont(s, s0)
          possibleFrames = helper.getPossibleStackFrames(s)
          (g, s1, littleStore, littlePStore) <- step(s, kont, possibleFrames, store, pStore)
        } yield (s1, Edge(s, g, s1), littleStore, littlePStore)
      //  } 
      val (obtainedStates, obtainedEdges, obtainedStores, obtainedPStores) = unzip4Components(newNodesEdgesStores) 
      // Transform switch edges to pairs of push/pop edges
      val noSwitchesEdges: Edges = if (canHaveSwitchFrames) processSwitchEdges(obtainedEdges) else obtainedEdges
      // Collect new states after decoupling switches
      val newStates0: Nodes = (if (canHaveSwitchFrames) {
        val nodes: Set[S] = (noSwitchesEdges -- ee).map {
          case Edge(source, _, target) => target
        }
        nodes ++ obtainedStates
      } else obtainedStates)
   
       val newStates = newStates0 -- ss
   
      val newEdges = noSwitchesEdges -- ee 
      
       val filteredStatesPossileVisits2 = 
    //   if (aggresiveCutOff) {
         newStates.filter {
        spvs =>
          {
            //helper.getEpsNextStates(spvs).isEmpty
            !spvs.weakerThanAny(helper.getEpsSuccsKeys, false) || (!spvs.weakerThanAny(helper.getEpsPredKeys, false))// strict equality in this case
          }
      }
    //   }
      // else{
       //  newStates
      // }
        
         val filteredStatesPossileVisits =  filteredStatesPossileVisits2
      
     //   println("newStates Size:" + newStates.size)
        //println("filetered out Size 2: " + filteredStatesPossileVisits2.size)
       // println("filetered out between " + filteredStatesPossileVisits.size)
        val possibleNewEdges  =
        if(aggresiveCutOff) {
          newEdges.filter(pe => {
        filteredStatesPossileVisits.contains(pe.target)
          //pe.target.weakerThanAny(filteredStatesPossileVisits, true)
      })
        }else
        {
          newEdges
        }
      
//      val filteredStatesPossileVisits = newStates.filter {
//        spvs =>
//          {
//            helper.getEpsNextStates(spvs).isEmpty
//           // !spvs.weakerThanAny(helper.getEpsSuccsKeys, true) // strict equality in this case
//          }
//      }
//     
//     //   println("filter out in next ECG states: ", filteredStatesPossileVisits.size)
//        
//      // and get the actual tovisit  edges
//      val possibleNewEdges = newEdges.filter(pe => {
//        filteredStatesPossileVisits.contains(pe.target)
//          //pe.target.weakerThanAny(filteredStatesPossileVisits, true)
//      })
 
      // update the ECG
      helper.update(possibleNewEdges)  
      val newStore: SharedStore = obtainedStores.foldLeft(store)(_ join _) //++ _) 
      val newPStore: PSharedStore = obtainedPStores.foldLeft(pStore)(_ join _) //++ _) 
      val storeSS = getStoreSensitiveStates(ss) 

      // get ECG nexts based on the filtered new states
       val epsNewNexts = 
     // if(aggresiveCutOff){
        filteredStatesPossileVisits.flatMap(s => {  
        val news = helper.getEpsNextStates(s) 
        news
      }) 
      
      // new to Visit has widened (filtered) weaker states
      val (newToVisit2, newEdges2) = 
        getStatesIfAco(helper, filteredStatesPossileVisits //newStates
        , possibleNewEdges, //newEdges,  
          epsNewNexts,
          true) //}

          val newToVisit = //newToVisit2 -- ss
               newToVisit2.foldLeft(Set[S]())((res, spsv) => {
           if(!spsv.weakerThanAny(ss, false))
               res + spsv
           else res
         }) 
          
    //  println("after aco, new states ", newToVisit.size)
     // println("after aco, new edges: ", newEdges2.size)
      val ss1: Nodes = ss ++ newStates//filteredStatesPossileVisits + // newStates + 
        s0

      val ee1 = (ee ++ newEdges)//possibleNewEdges)
      //newEdges)

      incNoEdges(possibleNewEdges.size)
      incNoStates(filteredStatesPossileVisits.size)

      val cond1 = !newEdges2.isEmpty //!newEdges.subsetOf(ee)  
      val cond2 = // Measure.measureTime ("partialOrderStoreCompare")
        //{
        !newStore.isSubsumedBy(store) // ! partialOrderStoreCompare(newStore, store) //}//(store !=   newStore) }

      val cond3 = !newPStore.isSubsumedBy(pStore) //!partialOrderStoreCompare(newPStore, pStore)

      val shouldProceed = cond1 || cond2 || cond3

     /* println("DSG: Nodes explored " + noEdgesExplored + " newEdges/possiblenew/newEdges2: " + newEdges.toList.length + "/" + possibleNewEdges.size + "/"
        + newEdges2.toList.length + " Possible toVisit States/filtertovisit/REALvisit states " +
        (newStates ++ epsNewNexts).toList.length + "/" + (filteredStatesPossileVisits ++ epsNewNexts).toList.length + "/" + newToVisit.toList.length + " \n")*/
      println("DSG Iteration..# to visit: " + newToVisit.toList.length)
      (DSG(ss1, ee1, s0), helper, shouldProceed, newToVisit, newStore, newPStore) //newStore)

    }
  }

  sealed class NewDSGHelper {

    import scala.collection.mutable.{ Map => MMap, HashMap => MHashMap }

    private val epsPreds: MMap[S, Nodes] = new MHashMap
    private val epsSuccs: MMap[S, Nodes] = new MHashMap
    private val topFrames: MMap[S, Set[Frame]] = new MHashMap

    def getEpsSuccsKeys = epsSuccs.keySet.toSet
    def getEpsPredKeys = epsPreds.keySet.toSet

    def printEpsNexts {
      //  epsSuccs.foreach(println)
      println(" +epslang size " + epsSuccs.size)
    }
    /**
     * Let s1 --[+f]--> s2_1 --> .... --> s2_n --[-f]--> s3
     * Then predForPushFrame((s2_i, f)) contains s1
     */
    private val predForPushFrame: MMap[(S, Frame), Nodes] = new MHashMap
    private val nonEpsPreds: MMap[S, Nodes] = new MHashMap
    private val possibleStackFrames: MMap[S, Set[Frame]] = new MHashMap

    ////////////////// Public methods //////////////////

    def update(newEdges: Set[Edge]) {
      //println("update helper:")
      for (e <- newEdges) {
        e match {
          case Edge(s1, Eps, s2) => equalize(s1, s2)
          case Edge(s1, Pop(f), s2) => processPop(s1, f, s2)
          case Edge(s1, Push(f), s2) => processPush(s1, f, s2)
          case Edge(_, se @ Switch(_, _, _), _) => throw new DSGException("Illegal switch edge: " + se)
        }
      }
    }

    /* def filterNewStatesInECG(news: Set[S]) : Set[S] = {
      news.filter{
        s => (epsPreds.contains(s) || epsSuccs.contains(s))
      }
    }
    
    def inECG(s: S): Boolean = {
      epsPreds.contains(s) || epsSuccs.contains(s)
    }*/

    /**
     *
     * Constructs a fake continuation with only a top frame (if any)
     */
    def getRequiredKont(s: S, s0: S): Set[Kont] = {
      val frames = gets(topFrames, s)
      if (frames.isEmpty) {
        Set(List())
      } else {
        // the current control state is finalState,
        // which will return true
        if (mustHaveOnlyEmptyContinuation(s)) {
          Set(List())

          /**
           * (REMARK)
           * [Valid final candidate]
           * Should carry a value and be epsilon-reachable
           * from the initial state
           */
        } else if (canHaveEmptyContinuation(s)
          && (getEpsPredStates(s)).contains(s0)) {
          frames.map(f => List(f)) + List()
        } else {
          frames.map(f => List(f))
        }
      }
    }

    /**
     * Necessary for abstract GC
     *
     * (REMARK)
     * [Dyck property exploited]
     * Compute recursively all possible frames that can be
     * somewhere in the stack for a state 's'
     */
    def getPossibleStackFrames(s: S): Kont = gets(possibleStackFrames, s).toList
    /*{
      if (!shouldGC) {
        // We don't deed it if there is no --gc flag
        return Nil
      }

      // initial -- just top frames
      var workSet: Nodes = Set(s) ++ getEpsPredStates(s)

      // first iteration
      var frames = workSet.flatMap(s => gets(topFrames, s))

      // get non-eps preds
      val neps = workSet.flatMap(x => nonEpsPreds.getOrElse(x, Set()))
      val toProcess = neps ++ neps.flatMap(getEpsPredStates(_))
      var newWorkSet: Nodes = workSet ++ toProcess

      def iterate(delta: Nodes) {
        if (!workSet.equals(newWorkSet)) {
          // compute new frames
          frames = frames ++ delta.flatMap(s => gets(topFrames, s))
          // update old working set
          workSet = newWorkSet
          // compute new states
          val neps1 = delta.flatMap(x => nonEpsPreds.getOrElse(x, Set()))
          val delta1 = neps1 ++ neps1.flatMap(getEpsPredStates(_))
          newWorkSet = workSet ++ delta1

          iterate(delta1)
        }
      }

      iterate(toProcess)
      frames.toList
    }*/

    def getEpsNextStates(s: S): Nodes = gets(epsSuccs, s)

    ///////////////// Inner methods ////////////////////

    // private 
    def getEpsPredStates(s: S): Nodes = gets(epsPreds, s)

    def updatePossibleStackFrames(s: S) {
      val possibleAsTop = gets(topFrames, s)
      puts(possibleStackFrames, s, possibleAsTop)
      // for all non-eps predecessors of s
      for (spred <- gets(nonEpsPreds, s) ++ gets(epsPreds, s)) {
        // see what are their possible stack-frames
        val newPossibleStackFrames = gets(possibleStackFrames, spred)
        // add them to possible stack frames of s
        puts(possibleStackFrames, s, newPossibleStackFrames)
      }
    }

    /**
     * "Equalize" eps-predecessors & eps-successors
     * when an eps-transition s1 --[eps]--> s2 is added
     */
    private def equalize(s1: S, s2: S) {
      val preds = Set(s1) ++ gets(epsPreds, s1)
      val nexts = Set(s2) ++ gets(epsSuccs, s2)

      // Add new successors
      for (s <- preds) {
        puts(epsSuccs, s, nexts)
      }

      // Add new predecessors and top frames
      val topFramesToAdd = preds.flatMap(x => gets(topFrames, x))
      for (s <- nexts) {
        puts(epsPreds, s, preds)
        puts(topFrames, s, topFramesToAdd)
        for (f <- gets(topFrames, s1)) {
          val predForPushForS1 = gets(predForPushFrame, (s1, f))
          puts(predForPushFrame, (s, f), predForPushForS1)
        }

        // Update introspective history for GC
        updatePossibleStackFrames(s)
      }
    }

    /**
     * Update topFrames and predForPushFrames for a new edge s1 --[+f]--> s2
     */
    private def processPush(s1: S, f: Frame, s2: S) {
      val nexts = Set(s2) ++ gets(epsSuccs, s2)
      for (s <- nexts) {
        puts(topFrames, s, Set(f))
        puts(predForPushFrame, (s, f), Set(s1))
        puts(nonEpsPreds, s, Set(s1))
        updatePossibleStackFrames(s)
      }
    }

    /**
     * Update eps-graphs for a new egde s1 --[-f]--> s2
     */
    private def processPop(s1: S, f: Frame, s2: S) {
      val newEpsPreds = gets(predForPushFrame, (s1, f))
      for (s <- newEpsPreds) {
        equalize(s, s2)
      }
    }

    /**
     * Utility function for multimaps
     */
    private def puts[A, B](map: MMap[A, Set[B]], key: A, newVals: Set[B]) {
      val oldVals = map.getOrElse(key, Set())
      val values = oldVals ++ newVals
      map += ((key, values))
    }

    private def gets[A, B](map: MMap[A, Set[B]], key: A): Set[B] = map.getOrElse(key, Set())

  }
  private def getStoreSensitiveStates(ss: Set[S]) = ss.filter(isStoreSensitive(_))

  /**
   * ************************************************************
   * Some utility methods
   * *************************************************************
   */

  /**
   * The function exploits the balanced structure of paths in DSG
   * So any "new" stack action cannon affect the status of successor nodes,
   * only "close" predecessors might become epsilon-predecessors.
   */
  def stackActionsEquivalent(g1: Frame, g: Frame): Boolean = {
    g1 == g
  }

  private def processSwitchEdges(edges: Edges): Edges = edges.flatMap {
    case Edge(source, Switch(popped, target: S, pushed), mid) => Set(
      Edge(source, Pop(popped), mid),
      Edge(mid, Push(pushed), target))
    case e => Set(e)
  }

  /**
   * Prints DSG according to the passed parameters
   */

  def prettyPrintDSG2(dsg: DSG): String = {

    val edges = dsg.edges

    val states: Set[ControlState] = dsg.nodes.asInstanceOf[Set[ControlState]]

    var stateCounter = 0
    val map: Map[ControlState, Int] = states.map(s => {
      stateCounter = stateCounter + 1
      (s, stateCounter)
    }).toMap

    val buffer = new StringBuffer
    buffer.append("digraph BST {\nsize=\"6,4\" \n ")

    var list: List[String] = List()
    for (Edge(s, g, s1) <- edges if s != s1) {
      val buf = new StringBuffer()
      buf.append("\"" + prettyPrintState2(s, map) + "\"")
      buf.append(" -> ")
      buf.append("\"" + prettyPrintState2(s1, map) + "\"")

      if (!simplify) {
        buf.append(" [label=\"")
        buf.append(StringUtils.truncateIfLong(g.toString, 100))
        buf.append("\"]")
      }

      buf.append(";\n")
      list = buf.toString :: list
    }

    buffer.append(list.distinct.mkString(""))
    buffer.append("}\n")

    buffer.toString
  }

  import org.ucombinator.utils.StringUtils._

  def dumpDSGGraph2(resultDSG: DSG): String = {

    import java.io._

    val graphs = new Directory(new File(graphsDirName))
    if (!graphs.exists) {
      graphs.createDirectory(force = true)
      graphs.createFile(failIfExists = false)
    }

    val subfolderPath = graphsDirName + File.separator + StringUtils.trimFileName(".")
    val subfolder = new Directory(new File(subfolderPath))
    if (!subfolder.exists) {
      subfolder.createDirectory(force = true)
      subfolder.createFile(failIfExists = false)
    }

    val path = subfolderPath + File.separator + (new java.util.Date()).getTime + ".gv"
    val file = new File(path)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)
    writer.write(prettyPrintDSG2(resultDSG))
    writer.close()
    path
  }

}
class DSGException(s: String) extends Exception(s)