package org.ucombinator.godelhash.numbertheory

object FasterPrimeGen {
 

 /* case class Cross(next: Int, incr: Int)

def adjustCrosses(crosses: List[Cross], current: Int) = {
  def nextIncr = crosses collect { 
    case cross @ Cross(`current`, incr) => cross copy (next = current + incr)
  }

  def unchangedCrosses = crosses filter (_.next != current)

  nextIncr ::: unchangedCrosses
}

def notPrime(crosses: List[Cross], current: Int) = crosses exists (_.next == current)

def sieve(s: Stream[Int], crosses: List[Cross]): Stream[Int] = {
    val current #:: rest = s

    if (notPrime(crosses, current)) sieve(rest, adjustCrosses(crosses, current))
    else current #:: sieve(rest, Cross(current * current, current) :: crosses)
}

val primes = sieve(Stream from 2, Nil)*/

  def sieve(s: Stream[Int]): Stream[Int] = {
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  }

  // All primes as a lazy sequence
  val primes = sieve(Stream.from(2))


    def main(args: Array[String]) {
      println(primes.take(100000).toList) //note that indexes are zero-based 
      // shit quickly run out of memory!!!
    }

   
}