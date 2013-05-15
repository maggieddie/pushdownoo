package org.ucombinator.dalvik.informationflow
import scala.tools.nsc.io.File
import org.ucombinator.utils.StringUtils
import org.ucombinator.playhelpers.AnalysisHelperThread
import models.PermissionPair
import scala.collection.mutable.Map
import org.ucombinator.utils.AIOptions
import scala.tools.nsc.io.Directory

/**
 * maintaining the source and sinks strings, as well as sensitive string values
 * 
 * The source and sink files is separated for the sake of clarity
 * 
 * The taint propagation rules are in StackCESKMachinary
 */
object DalInformationFlow { 
  
  
    val taintKinds = Set("sdcard",
    				     "filesystem",
    				     "picture",
    				     "network", 
    				     "location",
    				     "sms",
    				     "phone",
    				     "tobedetermined",
    				     "voice",
    				     "display","executable", "timeordate", 
    				     "reflection", "deviceid",   "browserbosokmark", 
    				     "browserhistory", "thread", "ipc",
    				      "contact", "sensor" ,"account" ,"media" ,"serialid", "random")
 
	//var sources: Set[(String, String)] = Set[(String, String)]()
	//var sinks: Set[(String, String)] = Set[(String, String)]()

    				     
	// contains complete files names, (like /sdcard)
	// http://, .cc
	//var sensitiveStrings: Set[(String, String)] = Set[(String, String)]()

    				      
	def decideTaintKinds(name: String) : Set[String] = {
	  val kindsFromSources =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].sources.foldLeft(Set[String]())((res: Set[String], p) => {
	    val str = p._1
	    val kind = p._2
	    if (str == name)  
	          res + kind 
	          else res
	  })
	   val kindsFromSinks =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].sinks.foldLeft(Set[String]())((res: Set[String], p) => {
	    val str = p._1
	    val kind = p._2
	    if (str == name)  
	          res + kind 
	          else res
	  })
	  
	  val res = kindsFromSources ++ kindsFromSinks
	 
	  res 
	}
	
	def isSensitiveStr(input: String) : Boolean = {
	 
	  val individuals =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].sensitiveStrings.filter((recStr) => {
	    
	    input.contains(recStr._1)
	  })
	  val strSs = Thread.currentThread().asInstanceOf[AnalysisHelperThread].sensitiveStrings.map(_._1)
	  val cond1 = strSs.contains(input) 
	  val cond2 = !individuals.isEmpty 
	  
	  cond1 || cond2
	 
	}
	
	def getTaintKindsForString(input: String) : Set[String] = {
	    val kindsFromSs = Thread.currentThread().asInstanceOf[AnalysisHelperThread].sensitiveStrings.foldLeft(Set[String]())((res: Set[String], p) => {
	    val str = p._1
	    val kind = p._2
	    if (input.contains(str))  
	          res + kind 
	          else res
	  })
	 
	  kindsFromSs
	}
	
	def decideSourceOrSinkLevel(name: String): Int = {
	 // println("sdfsfgsdlkhfjksdgjshkdadfsjdklssfkjdla;a")
	  val strsSources = Thread.currentThread().asInstanceOf[AnalysisHelperThread].sources.map(_._1)
	  val inSource = strsSources.contains(name)
	  val individualSrcs =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].sources.filter((src) => {
	    val name2 = StringUtils.getMethNameFromMethPath(name)
	    name2.contains(src._1)
	  })
	  val srcCond2 =  !individualSrcs.isEmpty 
	  
	  val strsSinks = Thread.currentThread().asInstanceOf[AnalysisHelperThread].sinks.map(_._1)
	  val inSink  = strsSinks.contains(name)
	  
	  val individualSinkss =  Thread.currentThread().asInstanceOf[AnalysisHelperThread].sinks.filter((src) => {
	   val name2 = StringUtils.getMethNameFromMethPath(name)
	    name2.contains(src._1)
	  })
	  val sinkCond2 =  !individualSinkss.isEmpty 
	 
	  
	  val isSrc = (inSource || srcCond2)  
	  val isSink =  (inSink || sinkCond2)
	  
	  if(isSrc && isSink) 3
	  else if(isSrc == true && isSink == false) 1
	  else if(isSrc == false && isSink == true) 2
	  else 0
	}
	
	
	
	 private def parseInRawPermMap : scala.collection.mutable.Map[String, PermissionPair] = {
     val permMapFilePath  =  "android-knowledge" + File.separator + "permission-map.txt" 
     
     val classLines =  File(permMapFilePath).lines.toList.filter(_.trim() !=  "" )
     val deduplicateClsLines = classLines.toSet.toList
     
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
	 
	 private def getAccessedPerm : List[String] = {
	   val curPermMap = Thread.currentThread().asInstanceOf[AnalysisHelperThread].permissionMap
	   
	   curPermMap.flatMap {
	     case (apiName, pp) => {
	       if(pp.isAccessed)
	    	   pp.perms
	    	   else List[String]()
	     }
	   }.toList.toSet.toList
	 }

	 
  def dumpPermReport(opts: AIOptions) {
    import java.io.File
    import java.io.FileWriter
    
    val buffer = new StringBuffer()
    
    buffer.append("*****************************************\n")
    buffer.append("*					*\n")
    buffer.append("*	Least Permission Report		*\n")
    buffer.append("*					*\n")
    buffer.append("*****************************************\n\n\n")
    buffer.append("Permissions the app asked for (ASKED):\n")
    
    val askedPerms = Thread.currentThread().asInstanceOf[AnalysisHelperThread].declaredPerms

     askedPerms.foreach((ps) => {
      buffer.append(ps)
      buffer.append("\n")
    })
    
      buffer.append("Permissions detected by Anadroid:\n\n")
    val accessedPerms = getAccessedPerm 
    accessedPerms.foreach((ps) => {
      buffer.append(ps)
      buffer.append("\n")
    })
    
      buffer.append("Vulnerabilities:\n\n")
    val askedPermSet = askedPerms.toSet
    val accessedPermSet = accessedPerms.toSet
    
    val aMinusU = askedPermSet diff accessedPermSet
    val uMinusA = accessedPermSet diff askedPermSet
    
    val subset12 = askedPermSet subsetOf accessedPermSet
    val subset21 = accessedPermSet subsetOf askedPermSet  
    val equalSet = subset12 && subset21
    if(equalSet) {
          buffer.append("	ASKED = USED	\n")
    }else if(subset12) {
      buffer.append("	ASKED <= USED	\n")
       (accessedPermSet diff askedPermSet).foreach((p) => {
         buffer.append(p)
         buffer.append("\n")
       })
    }else if(subset21){
       buffer.append("	ASKED >= USED	\n")
       (askedPermSet diff accessedPermSet).foreach((p) => {
         buffer.append(p)
         buffer.append("\n")
       })
    } else {
    
    	if(!aMinusU.isEmpty){
    	  buffer.append("\n\n The following is not in USED but in ASKED\n")
     	aMinusU.foreach((p) => {
     	  buffer.append(p)
     	  buffer.append(" ")
     	})}
    	else if(! uMinusA.isEmpty){
     	   buffer.append("\n\n The following is not in ASKED but in USED\n")
     	uMinusA.foreach((p) => {
     	  buffer.append(p)
     	  buffer.append(" ")
     	})
     	
  	  }
    } 
    
    
    val permReportsDirName = opts.permReportsDirName //opts.apkProjDir + File.separator + statisticsDirName
    if (opts.dumpStatistics) {
      val statDir = new Directory(new File(permReportsDirName))
      if (!statDir.exists) {
        statDir.createDirectory(force = true)
        statDir.createFile(failIfExists = false)
      }

      /* val subfolderPath = statisticsDirName + File.separator + StringUtils.trimFileName(opts.sexprDir)
      val subfolder = new Directory(new File(subfolderPath))
      if (!subfolder.exists) {
        subfolder.createDirectory(force = true)
        subfolder.createFile(failIfExists = false)
      }*/
      val path = opts.permReportPath //stasticsDir + File.separator + CommonUtils.getStatisticsDumpFileName(opts) // or use opts.statsFilePath
      val file = new File(path)
      if (!file.exists()) {
        file.createNewFile()
      }
      val writer = new FileWriter(file)

      writer.write(buffer.toString)
      writer.close()

      println("Report dumped into: " + path)

      path
    } else ""
  }
	
	 def parseInAndroidKnowledge   {
		val srcPath  =  "android-knowledge" + File.separator + "sources.txt"
		val sinkPath = "android-knowledge" + File.separator + "sinks.txt" 
		val sstringPath = "android-knowledge" + File.separator + "sensitive-strings.txt"
		
		val sourcesLines =  File(srcPath).lines.toList.filter(_ != "")
		Thread.currentThread().asInstanceOf[AnalysisHelperThread].sources =  sourcesLines.map((ps)=>{
		  val pairStr = ps.split("\\s+").toList
		  (pairStr(0), pairStr(1))
		  
		}).toSet  //sourcesLines.toSet 
		 
		val sinkLines = File(sinkPath).lines.toList.filter(_ != "")
	    Thread.currentThread().asInstanceOf[AnalysisHelperThread].sinks = sinkLines.map((ps)=>{
		  val pairStr = ps.split("\\s+").toList
		  (pairStr(0), pairStr(1))
		  
		}).toSet 
	    
	    val ssLines = File(sstringPath).lines.toList.filter(_ != "")
	    Thread.currentThread().asInstanceOf[AnalysisHelperThread].sensitiveStrings = ssLines.map((ps)=>{
		  val pairStr = ps.split("\\s+").toList
		  (pairStr(0), pairStr(1))
		  
		}).toSet
		
		Thread.currentThread().asInstanceOf[AnalysisHelperThread].permissionMap = parseInRawPermMap 
		
		 
      
	}
}