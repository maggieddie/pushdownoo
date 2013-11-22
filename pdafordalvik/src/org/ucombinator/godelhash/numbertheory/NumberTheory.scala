/**
 *  @author shuying
 */

package  org.ucombinator.godelhash.numbertheory;
import org.ucombinator.playhelpers.AnalysisHelperThread


trait PrimeHashable {
  def primeHash : BigInt // Must be a prime.
}

trait CompositeHashable {
  def compositeHash : BigInt // Composite representing all components
}


trait PrimeTable[A] {

  import scala.collection.mutable.HashMap

  /**
 * The primes is referenced from thread local.
 */
  var current = //Primes.primes
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].Primes.primes

  private def nextPrime : BigInt = {
    val p = current.head
    current = current.tail
    p
  }

  private var primeOf : HashMap[A, BigInt] = new HashMap[A, BigInt]
  private var objectOf : HashMap[BigInt, A] = new HashMap[BigInt, A]

  def prime(a : A) : BigInt = {
    (primeOf get a) match {
      case Some(p) => p
      case None => {
        val p = nextPrime
        primeOf(a) = p
        p
      }
    }
  }
}




/**
 * The primes needs to be put in thread local to ensure thread safe.
 */

/*object Primes {

  private val confidence = 20

  
   * It jsut generates an infinite number of prime numbers, and
   * and we can take whatever when we want to use it.
   
  val primes : Stream[BigInt] = primesAfter(2)

  def primesAfter(n : BigInt) : Stream[BigInt] = {
    var p = n 

    if (n == 2) {
      return Stream.cons(2,primesAfter(3))
    }

    while (!p.isProbablePrime(confidence)) {
      p += 2
    }

    Stream.cons(p, primesAfter(p+2))
  }
}
*/

object NumberTheory {
}
