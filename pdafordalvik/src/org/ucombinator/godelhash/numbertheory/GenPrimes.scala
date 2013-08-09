package org.ucombinator.godelhash.numbertheory
import scala.collection.mutable.Queue
import scala.util.control.Breaks._
import java.io.FileWriter
import java.io.BufferedWriter

object GenPrimes {

  
  var primeTable: List[BigInt] = List[BigInt]()
  
  def isPrime(number : BigInt) : Boolean = {
    
    var i = 2
    while(i < (number /2 )){
      if(number % i == 0)
        false
      i += 1
    }
    true
    }
  
  def genPrimes(max :BigInt ) {
    var primes: Array[BigInt] = new Array[BigInt](max.toInt)
  
  
    var insert = 1;
    primes(0) = 2
    
    var i  = 3
    
    while(i < max){
      var prime = false
      var x = 0
      while(x< insert){
      breakable {
        if(i % primes(x) == 0) 
        {
          prime = true
          break;
        }}
        x +=1
      }
      if(!prime){
        primes(insert) = i
        insert += 1
      }
      i += 1
    }
    
    println("gener")
    primes.foreach(println)
    
    //copy 
    
    var j = 0
    primeTable = primes.filter(_ != null).toList
  
      } 
  
  
  def writePrimesToFile(numbersToGen: BigInt) { 
    
    
    val  file : FileWriter = new FileWriter("primes" + numbersToGen + ".txt")
    val buffer: BufferedWriter = new BufferedWriter(file)
    
    primeTable.foreach((p) => {
      val s = "" + p + "\n"
      buffer.write(s)
    }) 
    buffer.close() 
  }
  
  // still slow!!!
   def main (args : Array[String])  {
     
    // val res = FasterPrimeGen.primes(10000)// .foreach((p) => primeTable ::: List(p))
     genPrimes(1000000)
     println("primes generates")
     writePrimesToFile(1000000)
   }
  
}