/**
 * Godelhash backed up by SortedSet
 */
 

package org.ucombinator.godelhash.sortedimpl


//import org.ucombinator.godelhash.mathmatics.numbertheory.PrimeHashable
import scala.collection.immutable.TreeSet
import scala.collection.immutable.SortedSet
import org.ucombinator.godelhash.numbertheory.PrimeHashable

 class PrimeSet[A <% PrimeHashable with Ordered[A]] 
(val comp : BigInt, val members : SortedSet[A]) {
  
   implicit def primeFromBigInt(bi : BigInt) : PrimeHashable = (new PrimeHashable {
    val primeHash = bi
  })
  
  final def modop(a : BigInt, b : BigInt) = a % b
  
  def + (a : A) : PrimeSet[A] = {
    val h = a.primeHash
    if (comp % h == 0) {
      this
    } else {
      new PrimeSet[A](comp * h, members + a)
    }
  }
   
  def contains(h : PrimeHashable) = (comp %  h.primeHash) == 0
  
  def isSubsetOf (b : PrimeSet[A]) : Boolean = (b.comp % comp) == 0

  def union (set2 : PrimeSet[A]) : PrimeSet[A] = 
    new PrimeSet[A]((comp * set2.comp)/(comp gcd set2.comp), members ++ set2.members)

  def diff (set2: PrimeSet[A]) : PrimeSet[A] = 
    new PrimeSet[A](  comp/(comp gcd set2.comp),  members -- set2.members)
    
  def intersect(set2:PrimeSet[A]) : PrimeSet[A] = 
     new PrimeSet[A](comp gcd set2.comp, members intersect set2.members)
     
  def - (a : A) : PrimeSet[A] = {
    val h = a.primeHash
    if(comp % h == 0) new PrimeSet[A](comp/h, members -a)
    else
      this
  }
  
  def PSEqual(set2: PrimeSet[A]) : Boolean = {
   this.isSubsetOf(set2) && set2.isSubsetOf(this)
  } 
  
  override def hashCode() : Int = comp.hashCode()
  override def equals (o : Any) = o match {
    case a : PrimeSet[_] => a.comp == comp
  }

  override def toString = members.toString
}


object PrimeSet {
  
  def apply[A <% PrimeHashable with Ordered[A]] () =
    new PrimeSet(1, TreeSet[A]())
  
  def apply[A <% PrimeHashable with Ordered[A]] (vals : A*) = 
    new PrimeSet[A](vals.foldLeft (BigInt(1)) ((ans,v) => ans * v.primeHash),TreeSet[A]() ++ vals.toList)
  
     def apply[A <% PrimeHashable with Ordered[A]] ( mul: Boolean,vals : A*) = 
         new PrimeSet[A](vals.foldLeft (BigInt(1)) ((ans,v) => ans * v.primeHash),TreeSet[A]()  )
}
