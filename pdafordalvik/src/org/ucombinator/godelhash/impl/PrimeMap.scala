package org.ucombinator.godelhash.impl

class PrimeMap[K , V] (val comp : BigInt, private val map : Map[K,V]) (implicit val primeHasher : ((K,V)) => BigInt)  {

  def + (k : K, v : V) : PrimeMap[K,V] = {
    val h = primeHasher(k,v)
    if ((comp % h) == 0) { 
      this
    } else {
      new PrimeMap(comp * h, map + ((k,v)))
    }
  }

  def apply(k : K) = map(k)
  def get (k : K) : Option[V] = map.get(k)
  def getOrElse (k : K, dflt : V) = map.getOrElse(k,dflt)

  def ++ (kvs : Iterable[(K,V)]) : PrimeMap[K,V] =
    kvs.foldLeft (this) { case (res,(k,v)) => res + (k,v) }

  def compare (spm2 : PrimeMap[K,V]) = comp compare spm2.comp

  override def hashCode () = comp.hashCode()
  override def equals (o : Any) = o match {
    case a : PrimeMap[_,_] => a.comp == comp
  }

  override def toString = "<" + comp + "," + map.toString + ">"
}

object PrimeMap {
  def apply[K  , V] () (implicit primeHasher : ((K,V)) => BigInt) = 
    new PrimeMap[K,V] (1, Map.empty)
}