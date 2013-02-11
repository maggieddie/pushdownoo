package org.ucombinator.tests
import tools.nsc.io.File
import scala.util.matching.Regex
import org.ucombinator.utils.StringUtils

object TestString {

  
  def matchSS(input: String, pattern:Regex) : Boolean = {
    //println(pattern)
    val resList = (pattern findAllIn input).toList
    //println(resList)
    if(resList.isEmpty) false
    else true
  }
  
  def test() {
    val str = "http://google.com"
    val p = "(http|ftp)://(.*)\\.([a-z]+)"
     println(p.r findAllIn str)
  }
  
  
   def main(args: Array[String]): Unit = {
       
       val strFilePath  =  "android-knowledge" + File.separator + "str-pat.txt" 
     
     val classLines =  File(strFilePath).lines.toList.filter(_.trim() !=  "" )
     val deduplicateClsLines = classLines.toSet.toList
     deduplicateClsLines.foreach(println)
     val map = 
       deduplicateClsLines.foldLeft(Map[Regex, String]())((res, line) => {
       val splitted :List[String] = line.split("\\s+").toList 
       val pattern = new Regex(splitted(0))
     println(matchSS("http://www.foobar.com/foo.gzip", pattern))
       val cate = splitted(1)
       res +  (pattern -> cate) 
     })  
     
     val regexes = map.map(_._1).toList
     
   
    }
}