/*
 * 
 */

package org.ucombinator.domains


object StandardDomains {

  import CommonAbstractDomains._ 
  
  case class StdStore(val map : Map[Addr,D]) extends Store {
    lazy val toList = map.toList
    lazy val getMap = map
    def apply(l : Addr) = map(l)
    def get (l : Addr) : Option[D] = map.get(l)
    
    //def getOrElse (l : Addr, dflt : D) = map.getOrElse(l,dflt) 
    // I think the getOrElse should work in the way he knows that he is using the D (standard or Godel backed)
     def getOrElse (l : Addr) : D = map.getOrElse(l, StandardDomains.botD) 
   
     
     // this <= otherStore
     
    def isSubsumedBy(otherStore : Store) : Boolean = {
      for ((l1,d1) <- map) {
        otherStore get l1 match {
          case Some (d2) => if (!(d1 isSubsumedBy d2)) return false
          case None => return false
        }
      }
      return true
    }
    
    def equal (s2: Store ): Boolean = {
     for ((l1,d1) <- map) {
        s2 get l1 match {
          case Some (d2) => if (!(d1 isSubsumedBy d2) || !(d2 isSubsumedBy d1)  ) return false
          case None => return false
        }
      }
      return true
    }

    def + (loc : Addr, d : D) : Store = {
      map get loc match {
        case Some(d2) => new StdStore(  map + (loc -> (d join d2)))//map(loc) = d join d2)
        case None => new StdStore(map + (loc -> d)) //(loc) = d)
      }
    }
    
    def ++ (lds : Iterable[(Addr,D)]) : Store = {
      
      lds.foldLeft(this : Store)((accum, pair) => {
        val (a, vs) = pair
        val oldVals : D = accum.getOrElse(a)
        val newVals : D = oldVals join vs.filterNot(UnspecifiedVal)
        accum + (a, newVals)
      }) 
      
      /*
      var cur : Store = this
      for ((l,d) <- lds) {
        cur = cur + (l,d) /// jue dui cuo de 
      }
      cur*/
    }

    def join (otherStore : Store) : Store = {
      val s2 = otherStore.asInstanceOf[StdStore]
      s2 ++ map
    }
    
     def mkDomainD (vals: Value*) : D = {
      new StdD(vals.toSet[Value])
    }
     
      def mkEmptyStore : Store = {
          botStore
        }
      
    
    
  }

  val botStore = new StdStore( Map[Addr,D]())


  case class StdD( set : Set[Value]) extends D {
   lazy  val size = set.size
    def isEmpty = size ==0
    lazy val toList = set.toList
    lazy val toSet = set
    def + (v : Value) : D =   new StdD(set + v)

    def join (otherD : D) : D = {
      val d2 = otherD.asInstanceOf[StdD]
      new StdD(set ++ d2.set)
    }
    
    // this <= otherD
    def isSubsumedBy(otherD : D) : Boolean = {
      // TODO: Make this smarter in case D is not flat
      /*for (v <- set) {
        if (!otherD.toList.contains(v))
          return false
      }
      return true*/
      this.set subsetOf otherD.asInstanceOf[StdD].set
    }
    
    
    
    def filterNot(valToAbandom: Value) : D ={
      // TODO: Potential memory leak! 
      //Now I realize I need some Cachine/Interning technique to manage the creation of all the objects!!!!
      new StdD(set.filter(ss => {ss!=valToAbandom}))
    }
  
   
    
    // for taint store
    def srcOrSinksSecurityValues  : D = {
      val res = 
     this.set.filter((oneV)
       => {
       oneV match {
         case Location  |
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
         	  DThread   |
         	  IPC  | 
         	  AMedia | 
         	  ASerialID | 
         	  AAccount  | 
         	  ASensor  | 
         	  AContact  | 
         	  ARandom | 
         	  ADB 
         	  => {
         	    true
         	  }
         case _ => false
       }
     }) 
     new StdD(res)
    }
  }

  val botD = new StdD(Set[Value]())
  
  
/*  class StdBEnv(val map : Map[Var,Addr]) extends BEnv {
    def + (v : Var, l : Addr) : BEnv = new StdBEnv(map(v) = l)

    def ++ (vls : Iterable[(Var,Addr)]) : BEnv = new StdBEnv(map ++ vls)

    def apply(k : Var) : Addr  = map(k)
    def get (k : Var) : Option[Addr] = map.get(k)
    def getOrElse (k : Var, dflt : Addr) : Addr = map.getOrElse(k,dflt)

    def compare (otherBEnv : BEnv) : Int = {
      var lst1 = map.toList
      var lst2 = otherBEnv.asInstanceOf[StdBEnv].map.toList

      while (!lst1.isEmpty) {
        if (lst2.isEmpty)
          return 1
        
        val (v1,l1) = lst1.head
        val (v2,l2) = lst2.head

        val cmp1 = v1 compare v2
        
        if (cmp1 != 0)
          return cmp1
        
        val cmp2 = l1 compare l2

        if (cmp2 != 0)
          return cmp2

        lst1 = lst1.tail
        lst2 = lst2.tail
      }

      if (!lst2.isEmpty)
        return -1 

      0
    }

    override lazy val hashCode : Int = map.hashCode() 
    override def equals(a : Any) : Boolean = (this compare a.asInstanceOf[BEnv]) == 0
  }

  val botBEnv = new StdBEnv(new TreeMap[Var,Addr])*/ 
  

}