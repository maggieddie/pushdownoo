package org.ucombinator.domains
import org.ucombinator.godelhash.numbertheory.PrimeHashable
import org.ucombinator.godelhash.numbertheory.PrimeTable


object CommonAbstractDomains {
  
  // addr
  
  // value
  
  //D
  
  //Store
  
  
   
// not yet.
 /* trait MachineState {
    def next : List[MachineState]
    def isCacheable : Boolean
  }
*/
    
  case class IntentExtraKeyTypeAndValue (keyType: String, keyVal: String)
  
  trait Addr extends PrimeHashable   {
    
    //def localCompare (loc2 : Addr) : Int ;
    
   /* def compare (l2 : Addr) : Int = {
      val c1 = this.getClass().getName().compare(l2.getClass.getName()) 

      if (c1 != 0)
        return c1

      localCompare(l2)
    }*/
    
    lazy val primeHash = Addr.prime(this)
  //  override def hashCode() : Int = throw new Exception("Loc hashCode method undefined")
   // override def equals(a : Any) : Boolean = throw new Exception("Loc equals method undefined")
  } 
  
  
  object Addr extends PrimeTable[Addr] { //with Canonicalizer[Loc] with EqualityOrder[Loc] {
    //private var maxNat = 0

   // def allocNat() : Addr = {
   //   maxNat += 1
   //   Addr(NatLoc(maxNat))
  //  }
  }
/*  case class NatLoc (n : Int) extends Loc {
    def localCompare (l2 : Loc) : Int = l2 match {
      case NatLoc(n2) => n compare n2
    }

    override def hashCode() = n
    override def equals(o : Any) = o match {
      case NatLoc(n2) => n == n2
      case _ => false
    }
  }*/


 // object VarLocCache extends PrimeTable[(Var,Loc)]

 // implicit def bindPrimeHash(vl : (Var,Loc)) : BigInt = VarLocCache.prime(vl._1, vl._2)

  /*trait BEnv extends Ordered[BEnv] {
    def + (v : Var, loc : Loc) : BEnv ;
    def ++ (vls : Iterable[(Var,Loc)]) : BEnv ;

    def apply(k : Var) : Loc ;
    def get (k : Var) : Option[Loc] ;
    def getOrElse (k : Var, dflt : Loc) : Loc ;

    override def hashCode() : Int = throw new Exception("hashCode() not implemented in BEnv")
    override def equals(a : Any) : Boolean = throw new Exception("equals() not implemented in BEnv")
  }
*/


  object Value extends PrimeTable[Value] //with Canonicalizer[Val] 
  trait Value extends PrimeHashable  { //with Ordered[Val] {
    lazy val primeHash = Value.prime(this)

  /* def localCompare (val2 : Value) : Int ; 
    
    def compare (val2 : Value) : Int = {
      val c1 = this.getClass().getName().compare(val2.getClass.getName())

      if (c1 != 0)
        return c1

      localCompare(val2)
    }

    override def hashCode() : Int = throw new Exception("hashCode not implemented")
    override def equals (a : Any) = (this compare a.asInstanceOf[Value]) == 0*/
  }
  
  
  
  
  
  case class TrueValue() extends Value
  case class FalseValue() extends Value
  case object BoolTop extends Value
  case class NullValue() extends Value
  
  case class VoidValue() extends Value
  
  /**
   * No sophisticated abstraction for int, float or double.
   * or we just use the top, don't care whether it is int,
   * long, or Bigint
   * 
   */
  abstract class AbstractNumLit extends Value
  case class NumLit(n: BigInt) extends AbstractNumLit
  case object NumTop extends AbstractNumLit
  def mkNumLit(n: BigInt): AbstractNumLit =  NumTop 
 /* = {
    if (n > 2) {
      NumTop
    } else if (n < -2) {
      NumTop
    } else {
      NumLit(n)
    }
  }*/
  
  case class IntValue(val v: BigInt) extends Value {
    def value = v
  }
  
  /**
   * FOr simplest string abstraction
   */
  abstract  class AbstractStringLiteral extends Value
  case class StringLit(str: String) extends AbstractStringLiteral
  case object StringTop extends AbstractStringLiteral
  
  
  // almost equal to the top of all values
  case object UnspecifiedVal extends Value
  
  // no context information aded into hte abstract vvalues
  case object Location extends Value
  case object FileSystem extends Value
  case object Sms extends Value
  case object Phone extends Value
  case object Picture extends Value
  case object DeviceID extends Value
  case object Network extends Value
  case object TimeOrDate extends Value
  case object SdCard extends Value
  case object ExecutableStr extends Value
  case object Display extends Value
  case object Voice extends Value
  case object ToBeDeTermined extends Value
  case object Reflection extends Value
  case object BrowserBookmark extends Value
  case object BrowserHistory extends Value
  case object DThread extends Value 
  case object IPC extends Value
  case object AMedia extends Value
  case object ASerialID extends Value
  case object AAccount extends Value
  case object ASensor extends Value
  case object AContact extends Value
  case object ARandom extends Value
  case object ADB  extends Value  
  



/*  trait Proc extends Val
  trait Bas extends Val


  object Clo {
    def apply (lam : Lambda, env : BEnv) = (Val(new Clo(lam,env))).asInstanceOf[Clo]
    def unapply (clo : Clo) = Some((clo.lam,clo.env))
  }

  class Clo(val lam : Lambda, val env : BEnv) extends Proc {
    def localCompare(v : Val) : Int = {
      val clo2 = v.asInstanceOf[Clo]
      val c = lam compare clo2.lam
      if (c != 0)
        return c
      env compare clo2.env
    }
    override def hashCode() : Int = lam.hashCode() * env.hashCode()
    override def toString = "<" + lam + "," + env + ">"
  }*/





  
   



  // type D = PrimeSet[Val]
  /*
  object D extends PartialOrder[D] {
    override def lessThanOrEqual(d1 : D, d2 : D) : Boolean = (d2.comp % d1.comp) == 0
    override def join(d1 : D, d2 : D) : D = d1 join d2
  }
  */
  // Pattern matching on singleton values:
  object DSingle {
    def unapply (d : D) : Option[Value] = 
      if (d.size == 1) {
        Some(d.toList.head)
      } else {
        None
      }
  }

   trait D {
    
    def size : Int ;
    def toList : List[Value] ;
    
    def toSet : Set[Value]
    
   	override def toString = {
   	  toList.foldLeft(""){
   	    case (res, elem) => {
   	      res + ", " + elem
   	    }
   	  }
   	}

    def + (v : Value) : D ;
    
    def join (d2 : D) : D ;

    def isSubsumedBy (d2 : D) : Boolean ;
    
    //for tatintset
     def srcOrSinksSecurityValues : D  
    
     // helper functions
    //def mkDomainD (vals: Value*) : D
    
    def filterNot (valToAbandom: Value) : D; 
     
     def isEmpty : Boolean;
     
     def equal(d2: D) : Boolean = { 
      this.isSubsumedBy(d2) && d2.isSubsumedBy(this)  
     }
  }
  
  



  trait Store { 
    
    def apply(l : Addr) : D ;
    
    def get (l : Addr) : Option[D] ;
    
   // def getOrElse (l : Addr, dflt : D) ;
    def getOrElse(l: Addr) : D ;

    def + (l : Addr, d : D) : Store ;
    
    def ++ (lds : Iterable[(Addr,D)]) : Store ;
    def join (s2 : Store) : Store ;

    def isSubsumedBy(s2 : Store) : Boolean ;
    
    def toList : List[(Addr,D)]
    
    //for taint store
    def pStoreHasTaintVals  : Boolean = {
      
      val allVals = this.getMap.foldLeft(this.mkDomainD())((res: D, pair) => { 
      res join  pair._2
    })
    val secuVals = allVals.srcOrSinksSecurityValues
    secuVals.toList.length > 0
    }
    
    def mkDomainD (vals: Value*) : D
    
    def mkEmptyStore : Store
    
    def getMap: Map[Addr, D]
    
    def equal(s2: Store) : Boolean 
    
    override def toString = {
      getMap.foldLeft(""){
        case (res, (k,v)) => {
          res + "Addr: " + k   + "\n" + "Vals: " + v
        }
      }
    }
    
  }

}