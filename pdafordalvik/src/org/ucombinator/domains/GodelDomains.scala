/**
 * Godel Domains:
 * 	1. Denotable are a PrimeSet of Abstract values
 * 	2. Godel Store: Addr -> PrimeSet[Value]
 *  3. Godel Benv: No need right now.
 *
 * @author liangsy (shuying)
 */

package org.ucombinator.domains
import org.ucombinator.godelhash.impl.PrimeSet
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.godelhash.numbertheory.PrimeTable

object GodelDomains {

  // will be added to the 
  //  object Config {
  //    var useStoreCompHash = false
  //    var useDCompHash = false
  //  }

  import CommonAbstractDomains._

  case class GodelD(val set: PrimeSet[Value]) extends D {

    def +(v: Value) = new GodelD(set + v)

    lazy val size = set.members.size
    lazy val toList = set.members.toList
    lazy val toSet = set.members
    
    def isEmpty = size == 0

    def join(otherDenotable: D): D = {
      val d2 = otherDenotable.asInstanceOf[GodelD]
      new GodelD(set union d2.set)
    }

    def isSubsumedBy(d2: D): Boolean =
      set.isSubsetOf(d2.asInstanceOf[GodelD].set)

    ///////////
    def filterNot(valToAbandom: Value): D = {
      // TODO: Potential memory leak! 
      //Now I realize I need some Cachine/Interning technique to manage the creation of all the objects!!!!

      // not good to delete elements!!! 
      new GodelD(set - valToAbandom)
    }

    /*def filter(valToAbandom: Value) : D ={
      // TODO: Potential memory leak! 
      //Now I realize I need some Cachine/Interning technique to manage the creation of all the objects!!!!
      
      // not good to delete elements!!! 
      new GodelD(set - valToAbandom)
    }*/

    ///////////
    // for taint store
    def srcOrSinksSecurityValues: D = {
      val res =
        this.toList.filter((oneV) => {
          oneV match {
            case Location |
              FileSystem |
              Sms |
              Phone |
              Picture |
              DeviceID |
              Network |
              TimeOrDate |
              SdCard |
              Display |
              Voice |
              ToBeDeTermined |
              Reflection |
              BrowserBookmark |
              BrowserHistory |
              DThread |
              IPC |
              AMedia |
              ASerialID |
              AAccount |
              ASensor |
              AContact |
              ARandom |
              ADB => {
              true
            }
            case _ => false
          }
        })
      new GodelD(PrimeSet(res: _*))
    }
  }

  object GodelD {
    def apply(): GodelD = new GodelD(PrimeSet[Value]())
  }

  val botD = GodelD()

  // currently we don't need the GodelBenv
  /*  class GodelBEnv (val map : SortedPrimeMap[Var,Loc]) extends BEnv {
    def + (v : Var, loc : Loc) : BEnv = 
      new GodelBEnv(map + (v,loc))

    def ++ (vls : Iterable[(Var,Loc)]) : BEnv = 
      new GodelBEnv(map ++ vls)

    def apply(k : Var) : Loc  = map(k)
    def get (k : Var) : Option[Loc] = map.get(k)
    def getOrElse (k : Var, dflt : Loc) : Loc = map.getOrElse(k,dflt)

    def compare (anotherBEnv : BEnv) = 
      map.compare(anotherBEnv.asInstanceOf[GodelBEnv].map)
    override def hashCode() = map.hashCode()
    override def equals (a : Any) = map.equals(a.asInstanceOf[GodelBEnv].map)
  }
  
  val botBEnv = new GodelBEnv(SortedPrimeMap[Var,Loc]())*/

  private type StoreMap = Map[Addr, D]
  private object StoreElements extends PrimeTable[(Addr, Value)]

  case class GodelStore(val comp: BigInt, val map: Map[Addr, D]) extends Store {
    override def toString = map.toString

    lazy val getMap = map
    lazy val toList = map.toList

    private def compOf(l: Addr, d: D): BigInt = {
      var c: BigInt = 1

      for (v <- d.toList) {
        val tmp = StoreElements.prime(l, v)
        // println("currnet primt is; ", tmp)
        c = c * tmp
        // println(c + ": " + v)
      }
      //  println("new compof value returned: ", c)
      c
    }

    def +(l: Addr, d2: D): GodelStore = {
      val newComp = compOf(l, d2)

      if (comp % newComp == 0 && newComp != 1) {
        // println ("godel hashing +: no new" )
        return this // Nothing new.
      }

      val map_ = map get l match {
        case Some(d) => map + (l -> (d join d2)) //map(l) = d join d2
        case None => map + (l -> d2) //map(l) = d2
      }
      // println("map_ ", map_)
      new GodelStore((comp * newComp) / (comp gcd newComp), map_)
    }

    def ++(lds: Iterable[(Addr, D)]): GodelStore = {
      // println("in godel hashing: before ", this)

      val res =
        lds.foldLeft(this)((res, kv) => {
          val k = kv._1
          val vs = kv._2
          res + (k, vs)
        })
      //println("in godel hashing: after ++: ", res)
      res
    }
    /*lds.foldLeft (this) 
      { case (res,(k,d)) => res + (k,d) 
      
      }*/

    // m2 is larger
    private def joinMaps(map1: StoreMap, map2: StoreMap): StoreMap = {
      var m1: StoreMap = map1
      var m2: StoreMap = map2
      if (map2.size < map1.size) {
        val t = m1
        m1 = m2
        m2 = t
      }
      for ((l, d) <- m1) {
        m2 get l match {
          case Some(d2) => m2 = m2 + (l -> (d join d2)) //(m2(l) = d join d2)
          case None => ()
        }
      }
      m2
    }

    def join(otherStore: Store): GodelStore = {
      val s2 = otherStore.asInstanceOf[GodelStore]
      new GodelStore((comp * s2.comp) / (comp gcd s2.comp), joinMaps(map, s2.map))
    }

    def apply(l: Addr) = map(l)
    def get(l: Addr): Option[D] = map.get(l)

    //  def getOrElse (l : Addr, dflt : D) = map.getOrElse(l,dflt)
    // I think the getOrElse should work in the way he knows that he is using the D (standard or Godel backed)
    def getOrElse(l: Addr): D = map.getOrElse(l, GodelDomains.botD)

    def isSubsumedBy(otherStore: Store): Boolean = {
      val s2 = otherStore.asInstanceOf[GodelStore]
      s2.comp % comp == 0
    }

    def equal(otherStore: Store): Boolean = {
      val s2 = otherStore.asInstanceOf[GodelStore]
      s2.comp % comp == 0 && (comp % s2.comp == 0)
    }

    def mkDomainD(vals: Value*): D = {
      var d = botD
      for (v <- vals) {
        d = d + v
      }
      d
    }
    def mkEmptyStore: Store = {
      botStore
    }

    override def hashCode() = comp.hashCode()
    override def equals(a: Any) = a.asInstanceOf[GodelStore].comp == comp
  }

  object GodelStore {
    def apply() = new GodelStore(1, Map())
  }

  val botStore = GodelStore()
}