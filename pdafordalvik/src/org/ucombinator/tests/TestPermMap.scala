package org.ucombinator.tests
import tools.nsc.io.File
import scala.util.matching.Regex
import models.PermissionPair
import scala.collection.mutable.Map

object TestPermMap {

  /* private def parseInRawPermMap : Map[String, PermissionPair] = {
     val permMapFilePath  =  "android-knowledge" + File.separator + "permission-map.txt" 
     
     val classLines =  File(permMapFilePath).lines.toList.filter(_.trim() !=  "" )
     val deduplicateClsLines = classLines.toSet.toList
     deduplicateClsLines.foreach(println)
     deduplicateClsLines.foldLeft(Map[String, PermissionPair]())((res, line) => {
       val splitted :List[String] = line.split("\\s+").toList
       val  slashedApiName : String= splitted.head.replace(".", "/")
       val newList : List[String] = if(res.contains(slashedApiName)) {
         val cureListO = res get   slashedApiName 
         val cureList = cureListO match {
           case Some(pm) => pm.perms
           case None => List[String]()
         }
         val  unionPerms = cureList.toSet  ++  splitted.tail.toSet 
         unionPerms.toList
       }else splitted.tail
       
       val newPermUsePair = PermissionPair(newList)
       
       
       res + (slashedApiName -> newPermUsePair)
     })  
  }*/
   
    private def parseInRawPermMap2 : scala.collection.mutable.Map[String, PermissionPair] = {
     val permMapFilePath  =  "android-knowledge" + File.separator + "permission-map.txt" 
     
     val classLines =  File(permMapFilePath).lines.toList.filter(_.trim() !=  "" )
     val deduplicateClsLines = classLines.toSet.toList
     deduplicateClsLines.foreach(println)
     deduplicateClsLines.foldLeft(Map[String, PermissionPair]())((res, line) => {
       val splitted :List[String] = line.split("\\s+").toList
       val  slashedApiName : String= splitted.head.replace(".", "/")
       val newList : List[String] = if(res.contains(slashedApiName)) {
         val cureListO = res get   slashedApiName 
         val cureList = cureListO match {
           case Some(pm) => pm.perms
           case None => List[String]()
         }
         val  unionPerms = cureList.toSet  ++  splitted.tail.toSet 
         unionPerms.toList
       }else splitted.tail
       
       val newPermUsePair = PermissionPair(newList)
       
       
       res + (slashedApiName -> newPermUsePair)
     })  
  }
    def main(args: Array[String]): Unit = {
       
      parseInRawPermMap2.foreach(println)
    }
}