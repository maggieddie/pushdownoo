package org.ucombinator.tests
import tools.nsc.io.File
import org.ucombinator.utils.AIOptions

object FilterData {

  private def dotToSlash(path: String) : String = {
    val res = path.split("\\.").toList
    res.foldLeft("")((res, s) => {res + s + "/"})
  }
  
  private def parseInAndroidKnowledge//(opts: AIOptions) 
  :   List[String]= {
     val classPath  =  "android-knowledge" + File.separator + "rows-3.csv"
    
     
    
     val classLines =  File(classPath).lines.toList.filter(_ != "")
     val deduplicateClsLines = classLines.toSet.toList
     deduplicateClsLines.foreach(println)
     
      deduplicateClsLines.map( (s) => {
        val lineList = s.split("\\,").toList
        val className = dotToSlash(lineList(1))
        val methodName = dotToSlash(lineList(2))
        className + methodName
        
      }).foreach(println)
     deduplicateClsLines 
     
     
  }
  
  private def getSink {
      val classPath  =  "android-knowledge" + File.separator + "source2.txt" 
    
     val classLines =  File(classPath).lines.toList.filter(_ != "")
     val deduplicateClsLines = classLines.toSet.toList
     deduplicateClsLines.foreach(println)
      
     deduplicateClsLines 
  }
  
   def main(args: Array[String]): Unit = {
     //parseInAndroidKnowledge
     getSink
   }
    	
 
}