package org.ucombinator.godelhash.sortedimpl
import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap

class SortedPrimeMap[K <: Ordered[K], V] (val comp : BigInt, private val map : SortedMap[K,V]) (implicit val primeHasher : ((K,V)) => BigInt) extends Ordered[SortedPrimeMap[K,V]] {

  def + (k : K, v : V) : SortedPrimeMap[K,V] = {
    val h = primeHasher(k,v)
    if ((comp % h) == 0) { 
      this
    } else {
      new SortedPrimeMap(comp * h, map + ((k,v)))
    }
  }

  def apply(k : K) = map(k)
  def get (k : K) : Option[V] = map.get(k)
  def getOrElse (k : K, dflt : V) = map.getOrElse(k,dflt)

  def ++ (kvs : Iterable[(K,V)]) : SortedPrimeMap[K,V] =
    kvs.foldLeft (this) { case (res,(k,v)) => res + (k,v) }

  def compare (spm2 : SortedPrimeMap[K,V]) = comp compare spm2.comp

  override def hashCode () = comp.hashCode()
  override def equals (o : Any) = o match {
    case a : SortedPrimeMap[_,_] => a.comp == comp
  }

  override def toString = "<" + comp + "," + map.toString + ">"
}

object SortedPrimeMap {
  def apply[K <: Ordered[K], V] () (implicit primeHasher : ((K,V)) => BigInt) = 
    new SortedPrimeMap[K,V] (1, TreeMap.empty)
}