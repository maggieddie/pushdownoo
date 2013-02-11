package org.ucombinator.utils
import scala.tools.nsc.io.File
import scala.util.matching.Regex
import org.ucombinator.playhelpers.AnalysisHelperThread

object AnalysisScope {
  
  // the parsed in exclusive pattern will be put in thread local
  
  //var exculsiveLibPatterns : List[Regex]=  List()
 
  
  def isExclusive (name: String) : Boolean = {
     val res = Thread.currentThread().asInstanceOf[AnalysisHelperThread].exculsiveLibPatterns.foreach((pat) => {
       if( pat.findAllIn(name).size > 0) 
         return true
     })
     return false
  }
  
//   def isExclusive2 (name: String) : Boolean = {
//     val res = exculsiveLibPatterns.foreach{
//       case p => {
//         if(p.findAllIn(name).size > 0 ) {
//           println("returned true")
//           println(p)
//            return true
//         }
//          
//       } 
//     }
//     return false;
//   }
  

  def parseInExclusiveLibNames {//List[Regex] = {
    val strFilePath = "dat" + File.separator + "libExclusions.txt"

    val classLines = File(strFilePath).lines.toList.filter(_.trim() != "")
    val deduplicateClsLines = classLines.toSet.toList
    //deduplicateClsLines.foreach(println)

    val res = 
    deduplicateClsLines.foldLeft(List[Regex]())((res, line) => {

     val pattern = new Regex(line)

      pattern :: res

    })
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].exculsiveLibPatterns = res
   // exculsiveLibPatterns = res
  }
   
   // tests for reading 
	def main(args: Array[String]): Unit = { 
	  AnalysisScope.parseInExclusiveLibNames
	//  println(AnalysisScope.isExclusive2("java/io/BufferInputStream")) 
     
}
	}