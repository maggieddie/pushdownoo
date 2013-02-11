package org.ucombinator.dsg

import org.ucombinator.dsg.DyckStateGraphMachinery
import tools.nsc.io.Directory
import org.ucombinator.utils.{StringUtils, AIOptions, FancyOutput}


trait DSGAnalysisRunner {
  self: FancyOutput with DyckStateGraphMachinery =>

  import org.ucombinator.utils.StringUtils._
  
  // get he graph folder and created the folder if not exits
  def getGraphParentFolder(opts: AIOptions) : String = {
      import java.io._

    val graphs = new Directory(new File(graphsDirName))
    if (!graphs.exists) {
      graphs.createDirectory(force = true)
      graphs.createFile(failIfExists = false)
    }
      
    

    val subfolderPath = graphsDirName + File.separator + StringUtils.trimFileName(opts.sexprDir)
    val subfolder = new Directory(new File(subfolderPath))
    if (!subfolder.exists) {
      subfolder.createDirectory(force = true)
      subfolder.createFile(failIfExists = false)
    }

   subfolderPath //+ File.separator + getGraphDumpFileName(opts) 
   
  }

  def dumpDSGGraph(opts: AIOptions, resultDSG: DSG): String = {

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


    val graphFolderPath = getGraphParentFolder(opts) //
    val filePath = graphFolderPath+ File.separator + getGraphDumpFileName(opts) 
    val file = new File(filePath)
    if (!file.exists()) {
      file.createNewFile()
    }
    val writer = new FileWriter(file)
    writer.write(prettyPrintDSG(resultDSG, graphFolderPath))
    writer.close()
    filePath
  }
  
   /**
   * Prints DSG according to the passed parameters
   */
  def prettyPrintDSG(dsg: DSG, graphParentFolerPath: String): String = {

    val edges = dsg.edges
    val states: Set[S] = dsg.nodes.asInstanceOf[Set[S]]

    var stateCounter = 0
    val map: Map[S, Int] = states.map(s => {
      stateCounter = stateCounter + 1
      (s, stateCounter)
    }).toMap

    val buffer = new StringBuffer
    buffer.append("digraph BST {\n \n ")

    var list: List[String] = List()
    for (Edge(s, g, s1) <- edges if s != s1) {
      val buf = new StringBuffer()
      
      //nodes definte
      val node1 = prettyPrintState(s, map)
      val htmlPath = buildHtmlPath(s, map, graphParentFolerPath)
     // println(htmlPath)
      buf.append("\"" + node1 + "\"" + " [style=filled, fillcolor=2, colorscheme=set312, URL=\"" + map(s) + ".html" + "\"]")
       buf.append(";\n")
      writeStateToHtmlfile(s, map, htmlPath)
      
      val node2 = prettyPrintState(s1, map)
      val htmlPath2 = buildHtmlPath(s1, map, graphParentFolerPath)
      
      buf.append("\"" + node2 + "\"" + " [style=filled, fillcolor=2, colorscheme=set312 , URL=\"" + map(s1) + ".html" + "\"]")
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
        buf.append(truncateIfLong(g.toString, 100))
        buf.append("\"]")
      }

      buf.append(";\n")
      list = buf.toString :: list
    }

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
