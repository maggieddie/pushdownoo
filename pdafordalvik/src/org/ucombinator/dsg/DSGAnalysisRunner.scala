package org.ucombinator.dsg
 
import tools.nsc.io.Directory
import org.ucombinator.utils.{StringUtils, AIOptions, FancyOutput}
import org.apache.commons.lang3.StringEscapeUtils
import org.ucombinator.utils.CommonUtils
import sys.process._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import org.ucombinator.playhelpers.AnalysisHelperThread
import org.ucombinator.domains.CommonAbstractDomains.IntentExtraKeyTypeAndValue
import org.ucombinator.dalvik.syntax.Stmt


trait DSGAnalysisRunner {
  self: FancyOutput with DyckStateGraphMachinery =>

  import org.ucombinator.utils.StringUtils._
  
  // get he graph folder and created the folder if not exits
  def getGraphParentFolder(opts: AIOptions) : String = {
      import java.io._

      val apkGraphDirName = opts.graphDirName //opts.apkProjDir +  File.separator + graphsDirName
    val graphs = new Directory(new File(apkGraphDirName))
    if (!graphs.exists) {
      graphs.createDirectory(force = true)
      graphs.createFile(failIfExists = false)
    }
     
    val subfolderPath = apkGraphDirName //+ File.separator + StringUtils.trimFileName(opts.sexprDir)
    val subfolder = new Directory(new File(subfolderPath))
    if (!subfolder.exists) {
      subfolder.createDirectory(force = true)
      subfolder.createFile(failIfExists = false)
    }

   subfolderPath //+ File.separator + getGraphDumpFileName(opts) 
   
  }
  
   //TODO:
  private def printIntentMapEntry(buffer: StringBuffer, map: Map[(String, String, Stmt), Map[String, Set[IntentExtraKeyTypeAndValue]]]) {
    map.foreach {
      case (newKey, fieldMap) => {
        if (!fieldMap.isEmpty) {
          buffer.append("Method: ", newKey)
          buffer.append("\n")
          fieldMap.foreach {
            case (op, vals) => {
              if (op.contains("getExtras")) {
                buffer.append("---Op: " + op + "\n---" + "(Key Type, Key Values)\n")
                vals.foreach((v) => buffer.append("(" + v.keyType + ", " + v.keyVal + ")" + "\n")) //buffer.append(v + "\n" ))
                buffer.append("\n")
              } else {
                buffer.append("---Op: " + op + "\n---" + "(Return Type, Args Values)\n")
                vals.foreach((v) => buffer.append("(" + v.keyType + ", " + v.keyVal + ")" + "\n")) //buffer.append(v + "\n" ))
                buffer.append("\n")
              }
            }
          }
        }
      }
  }
  }
  
  // TODO print the each DSG path
   def prettyPrintOneDSGStackPath(pathStack: List[Stack[Int]], map: Map[Int, S], opts: AIOptions, forIntentFuzzer:Boolean) : String = {
    
      val buf = new StringBuffer()
      if(!forIntentFuzzer) {
        
      pathStack.foreach {
        case ps => {
          val ite = ps.iterator
          while(ite.hasNext){
            buf.append(ite.next())
            if(ite.hasNext)
            	buf.append("->")
          }
          buf.append("\n\n")
        }
      }
      }else
      {
         pathStack.foreach {
           case ps => {
          val ite = ps.iterator
          while(ite.hasNext){
            val numS = ite.next()
            val state = if(map.contains(numS)) {Some(map(numS))}  else None
            state match{
              case Some(st) => {
                printIntentMapEntry(buf, st.intentRelatedInfo)
              }
              case None => {}
            }
           
            if(ite.hasNext)
            	buf.append("\n -> \n")
          }
          buf.append("\n\n")
        }
         }
        
      }
      buf.toString
      
   }
   
 
   private def isStateInIntentInforMap(state: S): Option[Map[(String, String, Stmt), Map[String, Set[IntentExtraKeyTypeAndValue]]]] = {
     val intentInfoMap  = Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap
     
     val stEqO = state.getStmtForEqual
     stEqO match {
       case Some(stEq) => {
         val clsPath = stEq.clsPath
         val methPath = stEq.methPath
         val oldStmt = stEq.oldStyleSt
         if(intentInfoMap.contains((clsPath, methPath, oldStmt))){
           //println("Found cls meth stmt: " + clsPath + methPath + oldStmt.toString)
           Some(Map((clsPath, methPath, oldStmt) -> intentInfoMap((clsPath, methPath, oldStmt))))
         }else {
            //println("NO Found cls meth stmt: \n" + (clsPath, methPath , oldStmt))
           None
         }
       }
       case None => {
         None
       }
     }
   }

  def prettyPrintOneDSGDirectPath(directPath: Set[List[Int]], map: Map[Int, S], opts: AIOptions, forIntentFuzzer: Boolean): String = {
    val buf = new StringBuffer()
      var  records = Map[List[Map[(String, String, Stmt), Map[String, Set[IntentExtraKeyTypeAndValue]]]], Boolean]()
   // println("path size: ", directPath)
    if (!forIntentFuzzer) {
      directPath.foreach {
        case ps => {
          val ite = ps.iterator
          while (ite.hasNext) {
            buf.append(ite.next())
            if (ite.hasNext)
              buf.append("->")
          }
          buf.append("\n\n")
        }
      }
    } else {
      directPath.foreach {
        case ps => {
         
          var nodesRelated = List[Map[(String, String, Stmt), Map[String, Set[IntentExtraKeyTypeAndValue]]]]()
          val ite = ps.iterator
          while (ite.hasNext) {
            val numS = ite.next()
            val state = if (map.contains(numS)) {
              Some(map(numS))
            } else None
            state match {
              case Some(st) => {
                val nmp = Thread.currentThread().asInstanceOf[AnalysisHelperThread].receivingIntentProcessingMap
                val resO = isStateInIntentInforMap(st)
                records
                resO match {
                  case Some(res) => {
                    nodesRelated = nodesRelated ::: List(res)
                   // printIntentMapEntry(buf, res)
                   // if (ite.hasNext)
                     // buf.append("->")
                  }
                  case None => {
                   // println("isStateINIntentInForMap returns None")
                  }
                }
              }
              case None => { //println("None State") 
                }
            }
          }
         if(!records.contains(nodesRelated)) 
           records += (nodesRelated -> true)
        }
        
      } // for each ends
      records.foreach {
        case (ks, v) => {
          if (!ks.isEmpty) {
            buf.append("New path starts!!\n")
            ks.foreach {
              case res => {
                printIntentMapEntry(buf, res)
                buf.append("->")
              }
            }
            buf.append("Path ends!!\n\n")
          }
        }
      }
    }
    buf.toString
  }
   
   def prettyPrintPaths(dsgs: List[DSG], graphParentFolerPath: String, opts: AIOptions, forIntentFuzzer: Boolean): String = {
 
    val buffer = new StringBuffer
    if(forIntentFuzzer) {
    	buffer.append(" All reachable paths that has intent operations: ")
    	buffer.append("\n\n")
    }else {
      buffer.append(" All reachable paths: ")
    	buffer.append("\n\n")
    }
    
    val (paths2, map) = flowPathsEachDSGDirectPaths(dsgs)//flowPathsEachDSG(dsgs)
       println("Path exploration on the analysed graph returns")
    var list: List[String] = List() 
    val pathSize = Thread.currentThread().asInstanceOf[AnalysisHelperThread].reachablePaths.size
    println("size ", pathSize)
     var records : Map[List[Int], Boolean]= Map.empty
    list = prettyPrintOneDSGDirectPath( 
        Thread.currentThread().asInstanceOf[AnalysisHelperThread].reachablePaths, 
        map ,opts, forIntentFuzzer) :: list
  
    //println("List: ", list)
    buffer.append(list.distinct.mkString(""))
    buffer.append("\n") 
   // println("path print finished")
    buffer.toString
    
    
  }
   
  def extractLongPaths(paths: List[List[Stack[Int]]]) : List[List[Stack[Int]]] = {
    paths.foldLeft(List[List[Stack[Int]]]())((res, path) => {
      //path is the paths for each DSG's paths 
      extractLongPathHelper(path.toSet).toList :: res
    })
  }
  
  private def pathNumToString (sp:Stack[Int]): String = {
    val ite = sp.iterator
    var str = ""
    while(ite.hasNext){
      str += ite.next()
    }
    str
  }
  
  // get the list elements that are subset of the current elem path to be added
  private def resListPathContainsElems(sp: Stack[Int], ls: Set[Stack[Int]]) : Set[Stack[Int]] = {
    val strSP = pathNumToString(sp)
		val res2 = 
		  ls.foldLeft(List[Stack[Int]]())((res, elemPath) => {
		    val elemPathStr = pathNumToString(elemPath)
		    if(strSP.contains(elemPathStr))
		      elemPath::res
		      else res
		  })
		  ls -- res2
  }
  
  private def pathInResList (sp: Stack[Int], ls: Set[Stack[Int]]): Boolean = {
		val strSP = pathNumToString(sp)
		val res = 
		  ls.filter((elemPath) => {
		  val elemPathStr = pathNumToString(elemPath)
		 elemPathStr.contains(strSP)
		})
		!res.isEmpty
  }
  
  def extractLongPathHelper(ls: Set[Stack[Int]]) : Set[Stack[Int]] = {
    var list = List[String]()
    var lstLst = Set[Stack[Int]]()
    //("lst length: ", ls.size)
    ls.foreach((path) => {
      if(lstLst.isEmpty) {
      //  println("loop 1")
        lstLst += path
      }
        else {
          if(!pathInResList(path, lstLst)){
           // println("loop 2, lstLength: ", lstLst.size)
             lstLst = resListPathContainsElems(path, lstLst)
             lstLst += path
          }
        }
    })
    // further filter out 1-> 1
    val lst2 = lstLst.filter((lstPath) => {
      val ss = lstPath.toSet
      ss.size != 1 })
   //lstLst
      lst2
  }
  
  

  def dumpSimplePaths(opts: AIOptions, resultsDSG: List[DSG]) {
    import java.io._
    val graphFolderPath = getGraphParentFolder(opts) //
    
      val lst = graphFolderPath.split("/").toList
      val apkName = lst(2)
      val statFileName = lst(lst.length-1)
      val file = new File("./test/" + apkName + "_" + 
          Thread.currentThread().asInstanceOf[AnalysisHelperThread].gopts.brCutoff + "_" +  statFileName + "_paths")
      
      if(!file.exists()) {
         file.createNewFile()
      }

    val writer = new FileWriter(file)
    // Change to print normal list path
    writer.write(prettyPrintPaths(resultsDSG, graphFolderPath, opts, false))
    writer.close()
  }
  
   def dumpPathsWithIntentsRelated(opts: AIOptions, resultsDSG: List[DSG]) {
    import java.io._
    val graphFolderPath = getGraphParentFolder(opts) //
    
      val lst = graphFolderPath.split("/").toList
      val apkName = lst(2)
      val statFileName = lst(lst.length-1)
      val file = new File("./test/" + apkName + "_" + 
          Thread.currentThread().asInstanceOf[AnalysisHelperThread].gopts.brCutoff + "_" +  statFileName + "_pathsWithIntentRelated" )
      if(!file.exists()) {
         file.createNewFile()
      }

    val writer = new FileWriter(file)
    writer.write(prettyPrintPaths(resultsDSG, graphFolderPath, opts, true))
    writer.close()
  }
  
  def dumpDSGGraph(opts: AIOptions, resultDSGs: List[DSG]): String = {

   import java.io._

   /* val graphs = new Directory(new File(graphsDirName))
    if (!graphs.exists) {
      graphs.createDirectory(force = true)
      graphs.createFile(failIfExists = false)
    }

    val subfolderPath = graphsDirName + File.separator + StringUtils.trimFileName(opts.sexprDir)
    val subfolder = new Directory(new File(subfolderPath))
    if (!subfolder.exists) {
      subfolder.createDirectory(force = true)
      subfolder.createFile(failIfExists = false)
    }*/


    val graphFolderPath =  getGraphParentFolder(opts) //
    val filePath = opts.dotFilePath// graphFolderPath+ File.separator +  //CommonUtils.getGraphDumpFileName(opts)  
    val file = new File(filePath)
    if (!file.exists()) {
      file.createNewFile()
    }
   
    val writer = new FileWriter(file)
    writer.write(prettyPrintDSGs(resultDSGs, graphFolderPath, opts))
    writer.close()
    
    // first tar dot file
        val graphZipCmdDot=   "/usr/bin/python ./pytar.py" + " " + opts.graphDirName + " graph.tar.gz"
       
    graphZipCmdDot !
    
    // val allTarCmdDot= "/usr/bin/python ./pytar.py" + " " + opts.apkProjDir + " all.tar.gz"
    //  graphZipCmdDot !
    
    // time to generate the svg file
    val dot2svgCmd :String = "/usr/local/bin/dot -Tsvg " + opts.dotFilePath + " -o " + opts.svgFilePath 
    
    dot2svgCmd !
    
    // zip the graph folder
   // val graphZipCmd = "cd " + opts.graphDirName + " && " + "tar -zcvf graph.tar.gz ./* "
      val graphZipCmd =   "/usr/bin/python ./pytar.py" + " " + opts.graphDirName + " graph.tar.gz"
       
    graphZipCmd !
    
    val allTarCmd = "/usr/bin/python ./pytar.py" + " " + opts.apkProjDir + " all.tar.gz"
     
      allTarCmd !
    
    filePath
  }
  
  
  private def defaultStyle(state:S) : String = {
     if (state.sourceOrSinkState )
       " [style=filled, fillcolor=red, colorscheme=set312, URL=\"" //red
    else if(state.taintedState)
     " [style=filled, fillcolor=4, colorscheme=set312, URL=\"" //origin
    else 
       " [style=filled, fillcolor=2, colorscheme=set312, URL=\"" //yello
  }
  //http://www.graphviz.org/doc/info/colors.html#brewer
  
  private def genStyleStringForState(state: S, opts:AIOptions): String = {
   
    if(opts.doRegex ||  opts.doCheckList){
      val checklists = opts.checkList
      val stateTaintKind = state.taintKind
      val toDoCheckListColor = checklists intersect stateTaintKind
      
      if(state.matchRegex(opts.regex) ){
         " [style=filled, fillcolor=8, colorscheme=rdpu8, URL=\""
      } 
      else if(!toDoCheckListColor.isEmpty){
        " [style=filled, fillcolor=7, colorscheme=rdpu8, URL=\""
      }
      else defaultStyle(state)
      
    }else  defaultStyle(state) 
    
  } 
  
  def prettyPrintOneDSG(dsg: DSG, graphParentFolerPath: String, map: Map[S, Int], opts:AIOptions) : String = {
    
      val edges = dsg.edges  
     // var list: List[String] = List()
       val buf = new StringBuffer()
      for (Edge(s, g, s1) <- edges if s != s1) { 
        
      //nodes definite
      val node1 = prettyPrintState(s, map)
      val htmlPath = buildHtmlPath(s, map, graphParentFolerPath)
      
      val stylStr1 = genStyleStringForState(s,opts)
      
      buf.append("\"" + node1 + "\"" + stylStr1 + map(s) + ".html" + "\"]")
       buf.append(";\n")
      writeStateToHtmlfile(s, map, htmlPath)
      
      val node2 = prettyPrintState(s1, map)
      val htmlPath2 = buildHtmlPath(s1, map, graphParentFolerPath)
      
      val stylStr2 = genStyleStringForState(s1,opts)
      
      buf.append("\"" + node2 + "\"" + stylStr2  + map(s1) + ".html" + "\"]")
      writeStateToHtmlfile(s1, map, htmlPath2)
      buf.append(";\n")
      
      // edges and style
       buf.append("\"" + node1+ "\"")
      buf.append(" -> ")
      buf.append("\"" + node2+ "\"")

      if (!simplify) {
         g match {
          case Eps =>  buf.append(" [style=dotted, ")
          case _ => buf.append(" [ ")
        }
       
        buf.append(" label=\"")
        buf.append(truncateIfLong(StringEscapeUtils.escapeJava(g.toString), 100))
        buf.append("\"]")
      } 
      buf.append(";\n") 
      }
      buf.toString 
        
  }
  
  
   def genSToCounter(dsgs: List[DSG]) : Map[S, Int] = {
     var stateCounter = 0 
     val listOfMap = 
       dsgs.map(dsg => {
    	 val states: Set[S] = dsg.nodes.asInstanceOf[Set[S]] 
    	 val map: Map[S, Int] = states.map(s => { 
    	   stateCounter = stateCounter + 1 
    	   (s, stateCounter) }).toMap
    	 map
     }).flatten.toMap 
     listOfMap
  }
   
   def genCounterToSMap(map: Map[S, Int]) : Map[Int, S] = {
     map.foldLeft(Map[Int, S]())((res, pair) => {
       val state = pair._1
       val counter = pair._2
       res + (counter -> state)
     })
   }

  def findNextStateIndexes(dsg: DSG, state: S, stateToIntMap: Map[S, Int]): Set[Int] = {
    val curStIndx = stateToIntMap(state)
    dsg.edges.foldLeft(Set[Int]())((res, eg) => {
      eg match {
        case Edge(s1, edge, s2) => {
          if (curStIndx == stateToIntMap(s1))
            res + stateToIntMap(s2)
          else
            res
        }
        case _ => res
      }
    })
  }

  /**
   * for each dsg, we will have the graph representation 
   * for each dsg, there is a list of path from init -> node
   * for all the dsg: List[List[Stack[Int]]]
   *  Actually, the path map could be Map[Int, List[Int]]
   */
  def flowPathsEachDSG(dsgs: List[DSG]): (List[List[Stack[Int]]],  Map[Int, S]) ={
  //(List[Map[Int, Set[Int]]], Map[S, Int], Map[Int, S])= {
    val map: Map[S, Int] = genSToCounter(dsgs)
    val integerToStateMap = genCounterToSMap(map) 
    
    val res = dsgs.foldLeft(List[List[Stack[Int]]]())((res, dsg) => {
      val nodes = dsg.nodes
      val curInitIndex = map(dsg.s0)
      val curNextIndexes =  findNextStateIndexes(dsg, dsg.s0, map)
     
      val marked = scala.collection.mutable.Map[Int, Int]()//Boolean] ()
      val edgeTo = scala.collection.mutable.Map[Int, Set[Int]] () // Int]()
      
      initInformation(marked, edgeTo, curNextIndexes) 
      // get the adjcent representtaion for the graph
      val oneGraphAdjReprentation = 
        nodes.foldLeft(Map[Int, Set[Int]]())((resMap, node) => {
      val curIndx = map(node)
      val nextStateIndexes = findNextStateIndexes(dsg, node, map) 
      if(!resMap.contains(curIndx))
    	  resMap + (curIndx -> nextStateIndexes)
      else 
        resMap + (curIndx -> ((resMap(curIndx) ++nextStateIndexes) ))
    })
    
    // dfs the paths starting from the init state of the current dsg
    edgeTo += (curInitIndex -> Set( curInitIndex))
    dfs(dsg, oneGraphAdjReprentation, curInitIndex, marked, edgeTo, map, integerToStateMap, List(curInitIndex))
    
//    val res4 = Thread.currentThread().asInstanceOf[AnalysisHelperThread].reachablePaths
//    res4.toList :: res
    
    val res2 = dsg.nodes.foldLeft(List[Stack[Int]]())((res3, node) =>{
      val toIndex = map(node)
      //println(marked)
      if(marked(toIndex)!= 0)  {//(marked(toIndex)){
        val pathToNodes = pathTo(edgeTo, marked, curInitIndex, toIndex)
        //println("pathToNodes size", pathToNodes.size)
        if(pathToNodes.isEmpty) res3
        else {
          res3 ::: pathToNodes
        }
      }
      else res3
    }) 
    	res2 :: res
    }) 
    
   (res, integerToStateMap)
  }
  
   def flowPathsEachDSGDirectPaths(dsgs: List[DSG]): (List[Set[List[Int]]],  Map[Int, S]) ={
  //(List[Map[Int, Set[Int]]], Map[S, Int], Map[Int, S])= {
    val map: Map[S, Int] = genSToCounter(dsgs)
    val integerToStateMap = genCounterToSMap(map) 
    
    val res = dsgs.foldLeft(List[Set[List[Int]]]())((res, dsg) => {
      val nodes = dsg.nodes
      val curInitIndex = map(dsg.s0)
      val curNextIndexes =  findNextStateIndexes(dsg, dsg.s0, map)
     
      val marked = scala.collection.mutable.Map[Int, Int]()//Boolean] ()
      val edgeTo = scala.collection.mutable.Map[Int, Set[Int]] () // Int]()
      
      initInformation(marked, edgeTo, curNextIndexes) 
      // get the adjcent representtaion for the graph
      val oneGraphAdjReprentation = 
        nodes.foldLeft(Map[Int, Set[Int]]())((resMap, node) => {
      val curIndx = map(node)
      val nextStateIndexes = findNextStateIndexes(dsg, node, map) 
      if(!resMap.contains(curIndx))
    	  resMap + (curIndx -> nextStateIndexes)
      else 
        resMap + (curIndx -> ((resMap(curIndx) ++nextStateIndexes) ))
    })
    
    dfs(dsg, oneGraphAdjReprentation, curInitIndex, marked, edgeTo, map, integerToStateMap, List(curInitIndex))
    
    val res4 = Thread.currentThread().asInstanceOf[AnalysisHelperThread].reachablePaths
    res4.toList :: res
    
    res4 :: res
    }) 
    
   (res, integerToStateMap)
  }
  
  private def forkedPathStack(preds: Set[Int], previousStack: Stack[Int]): List[Stack[Int]] = {
    preds.foldLeft(List[Stack[Int]]()) ((res, predIndx) => {
      val newPathStack = new Stack[Int]() ++ previousStack
      val nsp = newPathStack.push(predIndx)
      nsp :: res
      
    })
  }

  private def findPaths(lst: List[Stack[Int]], initIndex: Int,
    edgeTo: scala.collection.mutable.Map[Int, Set[Int]]): List[Stack[Int]] = {
    val newLst =
      lst.foldLeft(List[Stack[Int]]())((res, oneStackPath) => {
        val curIndex = oneStackPath.top
        if (curIndex != initIndex) {
          val preds = edgeTo(curIndex)
          if (!preds.contains(initIndex)) {
            if (preds.size > 1) {
              val newStackPaths = forkedPathStack(preds, oneStackPath)
              res ::: newStackPaths
            } else {
              if (!preds.isEmpty) {
                // don't change the oneStackPath!
                val newoneStackPath = new Stack[Int]() ++ oneStackPath  
                newoneStackPath.push(preds.toList.head)
                newoneStackPath :: res
              } else oneStackPath :: res
            }
          } else oneStackPath :: res
        } else oneStackPath :: res
      })
    if (newLst.toSet != lst.toSet) {
    //  println("looping here")
      findPaths(newLst, initIndex, edgeTo)
    }
    else {
      val newLst2 = newLst.foldLeft(List[Stack[Int]]()) ((res, elemPath) => {
        elemPath.push(initIndex) :: res
      })
      newLst2
      newLst
    }
  }
  
  // problem of the pathTO
  private def pathTo(edgeTo: scala.collection.mutable.Map[Int, Set[Int]],//Int], 
		  			markedMap: scala.collection.mutable.Map[Int, Int], //Boolean], 
		  			initIndex: Int, 
		  			toIndex: Int):  List[Stack[Int]] = {//Option[Stack[Int]] ={
  val res =  findPaths(List(new Stack[Int]().push(toIndex)), initIndex, edgeTo)
  res
  }
  
  private def initInformation(//marked: scala.collection.mutable.Map[Int, Boolean], 
      marked: scala.collection.mutable.Map[Int, Int],
      edgeTo:scala.collection.mutable.Map[Int, Set[Int]], nextIndexes: Set[Int]){
    nextIndexes.foreach {
      case index => {
        if( marked.contains(index) &&  marked(index) > 0)//marked(index))
        	marked += (index -> marked(index)) // true)
        else
          marked += (index -> 0) //false)
        
        if(edgeTo.contains(index) && !edgeTo(index).isEmpty) //!= -1)
        	edgeTo 
        else
          edgeTo += (index -> Set())//-1)
      }
    }
  }
  
  
  
  private def joinNumEdges(curIndex: Int,  nextIndex: Int, edgesTo: scala.collection.mutable.Map[Int, Set[Int]]) // : scala.collection.mutable.Map[Int, Set[Int]] = 
  {
    if(edgesTo.contains(nextIndex)) {
      edgesTo += (nextIndex -> (edgesTo(nextIndex) + curIndex))
    }else {
       edgesTo += (nextIndex -> Set(curIndex))
    }
    //edgesTo
  }
  
  private def accumarked(curIndex: Int, marked: scala.collection.mutable.Map[Int, Int]) {
    if(marked.contains(curIndex)) {
      
      marked += (curIndex -> (marked(curIndex) + 1))
    }
    else {
      marked += (curIndex -> 1)
    }
  }
  /**
   *
   */
  private def dfs(dsg: DSG, adjGraphMap: Map[Int, Set[Int]],
    curIndex: Int,
    //marked: scala.collection.mutable.Map[Int, Boolean] , 
    marked: scala.collection.mutable.Map[Int, Int],
    edgeTo: scala.collection.mutable.Map[Int, Set[Int]], map: Map[S, Int], indexToSMap: Map[Int, S],
    currentPath: List[Int]) {

    // during recursion, make sure the edges add first
    initInformation(marked, edgeTo, findNextStateIndexes(dsg, indexToSMap(curIndex), map))

    accumarked(curIndex, marked)
    val nextStateIndexes = adjGraphMap(curIndex)

    if (nextStateIndexes.isEmpty) {
      Thread.currentThread().asInstanceOf[AnalysisHelperThread].reachablePaths += currentPath
    } else {

      nextStateIndexes.foreach {
        case nextIndx => {
          val newLstPath = List[Int]() ++ currentPath
          val newLstPath2 = newLstPath ::: List(nextIndx)
          // you'd better ensure that it is inserted, it is initlized to contain!
          // marked has no use any more when the current 
          if ( //marked.contains(nextIndx) && 
          !currentPath.contains(nextIndx)) { //&& marked(nextIndx) == 0) {//!marked(nextIndx)) {
            //joinNumEdges(curIndex, nextIndx, edgeTo) //+= (nextIndx -> curIndex)

            dfs(dsg, adjGraphMap, nextIndx, marked, edgeTo, map, indexToSMap, newLstPath2)
          } else {
            //  val newTotalPaths0 = Set[List[Int]]() ++  Thread.currentThread().asInstanceOf[AnalysisHelperThread].reachablePaths
            Thread.currentThread().asInstanceOf[AnalysisHelperThread].reachablePaths += newLstPath2
            //dfs(dsg, adjGraphMap, nextIndx, marked, edgeTo, map, indexToSMap, currentPath)
          }
        }
      }
    }
  }
  
   /**
   * Prints DSG according to the passed parameters
   */
  def prettyPrintDSGs(dsgs: List[DSG], graphParentFolerPath: String, opts: AIOptions): String = {
 
    val map: Map[S, Int] = genSToCounter(dsgs) 
    val buffer = new StringBuffer
    buffer.append("digraph BST {\n \n ")
 
     var list: List[String] = List() 
     dsgs.foreach((dsg) => {
       list = prettyPrintOneDSG(dsg , graphParentFolerPath , map ,opts)  :: list
     }) 
    buffer.append(list.distinct.mkString("")) 
    buffer.append("}\n") 
    buffer.toString
  }
  
  private def buildHtmlPath(state: ControlState, map: Map[ControlState, Int], graphParentFolder: String) : String ={
   import java.io._
    graphParentFolder + File.separator + map(state) +".html"
  }
  
  
  def writeStateToHtmlfile(state: ControlState, map: Map[ControlState, Int], htmlPath: String) {
    import java.io._
   // val htmlPath = buildHtmlPath(state, map, graphParentFolder)//graphParentFolder + File.separator + map(state) +".html"
    val file = new File(htmlPath)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)
     writer.write(genPrettyStateToHtml(state, map))
    writer.close()
  }
 
}
