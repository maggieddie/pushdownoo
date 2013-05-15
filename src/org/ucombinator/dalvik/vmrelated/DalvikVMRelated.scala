package org.ucombinator.dalvik.vmrelated
import org.ucombinator.dalvik.syntax.{Stmt, DalvikClassDef, StmtNil, AExp,CompactMethodIndex, MethodDef, LineStmt}
import org.ucombinator.utils.{CommonUtils, Debug}
import org.ucombinator.utils.StringUtils
import scala.util.Random
import tools.nsc.io.File
import org.ucombinator.utils.AIOptions
import org.ucombinator.playhelpers.AnalysisHelperThread




//import org.ucombinator.dalvik.syntax.DalvikClassDef

trait DalvikVMRelated {
  	
  // the entry point need to init the objAExp
  case class EntryPoint(methodPath: String,  argTypes: List[String], body: Stmt, regsNum : BigInt)
  
  case class EntryPointInvokeStmt(entr: EntryPoint,objRegStr: String, nxt: Stmt, ln: Stmt, clsP:String, methP:String) extends Stmt {
    var next = nxt
    var lineNumber = ln
    var clsPath = clsP
    var methPath = methP
    override def toString = "EntryPointInvokeStmt: " +  entr.methodPath +"("+ objRegStr + " " + entr.argTypes + ")" +  StringUtils.stmtContextInfo(clsP, methP, lineNumber)
    
    //
    def refRegsStrSet : Set[String] = {
      val bd = entr.body
      
      val regNum = entr.regsNum
      val regsStrs = CommonUtils.getAllRegsStrFromRegNum(regNum)
       regsStrs.toSet ++ Set(objRegStr) ++ bd.refRegsStrSet
    }
    
    def defRegsStrSet: Set[String] = {
      Set()
    }
    
    def sourceOrSink = 0
    def taintKind = Set[String]()
  }
  
  case class InitEntryPointStmt(methodPath: String, argsTypes: List[String], body: Stmt, regsNum: BigInt,nxt: Stmt, ln: Stmt, clsP: String, methP: String) extends Stmt {
     var next = nxt
    var lineNumber = ln
    
    var clsPath = clsP
    var methPath = methP
    override def toString = "InitEntryPointStmt: " + methodPath +"(" + body + " " + regsNum + ")" 
    
    //
     def refRegsStrSet : Set[String] = {
       body.refRegsStrSet
    //    val thisRegStr = CommonUtils.getThisRegStr(regsNum, argsTypes.length)
     // Set(thisRegStr)
       
     //   val regsStrs = CommonUtils.getAllRegsStrFromRegNum(regsNum)
     //  regsStrs.toSet
       
    }
    
     // we say almost all the init arguments will be defined.
    def defRegsStrSet: Set[String] = {
     // body.defRegsStrSet
      val regsStrs = CommonUtils.getAllRegsStrFromRegNum(regsNum)
       regsStrs.toSet
    }
    
     def sourceOrSink = 0
     def taintKind = Set[String]()
  } 
  
  
   def getDeclaredPerms(opts: AIOptions)  : List[String] = {
	    val manifestFilePath = opts.apkProjDir + File.separator + "man_perms.txt" 
     
   
     val permLines =  File(manifestFilePath).lines.toList.filter(_.trim() !=  "")
     val deduplicateClsLines = permLines.toSet.toList 
	   deduplicateClsLines
	 }
  
  /**
   * will get the entry points from:
   * 1. from the implemented and overritten methods
   * 2. from the ones parsed from layout xml files
   */ 
  
  private def parseInAndroidKnowledge(opts: AIOptions) : (List[String], List[String], List[String]) = {
     val classPath  =  "android-knowledge" + File.separator + "classes.txt"
     val entryMethPath = "android-knowledge" + File.separator + "callbacks.txt"
     val xmlMethNames = opts.apkProjDir + File.separator + "handlers.txt" 
     
     println("the fxml d[ath: " +xmlMethNames)
     val classLines =  File(classPath).lines.toList.filter(_ != "")
     val deduplicateClsLines = classLines.toSet.toList
     val entryMethLines = File(entryMethPath).lines.toList.filter(_ != "")
     val dedupMethLines = entryMethLines.toSet.toList
     
     //val f = "./testapks" + File.separator + "Kittey"  +  File.separator + "Kittey.apk"  
     
     val handlerEntryFile = File(xmlMethNames)
    
    // if(handlerEntryFile.exists){
    //   println("the handler file exisits")
       val deduplicateHandlerEntries = handlerEntryFile.lines.toList.filter(_ != "").toSet.toList
       println("FOund entries from xmml file"+ deduplicateHandlerEntries)
       
     
      Thread.currentThread().asInstanceOf[AnalysisHelperThread].declaredPerms = getDeclaredPerms(opts)
      
       (deduplicateClsLines, entryMethLines, deduplicateHandlerEntries)
  }
  
  /**
   * retutrns the a list of a tuple of class def and the its entry  points. 
   * 
   * for the sensitive classes, then filter out al lthe interited and implemented methods
   * 
   * for now we think the entries points specified in layout xml file is also in those entry points component classes
   */
  private  def extractRawEntryPoints(opts: AIOptions) : List[(DalvikClassDef, List[EntryPoint])]  = {
   
    val (classLines, entries, xmlEntries) = parseInAndroidKnowledge(opts)
     
    
    Thread.currentThread().asInstanceOf[AnalysisHelperThread].classTable.foldLeft(List[(DalvikClassDef, List[EntryPoint])]()) {
      case (res, (className, clsDef) ) => {
        
        val clsName2 = if(className.contains("$")) className.split("\\$").toList.head
        else className 
        
        if(classLines.toSet.contains(clsName2)  || clsName2.startsWith("android/support/v4")) { // thsi says that the class is the based  
          res
        }else{
        val superClassStrs = clsDef.getSuperStrsIncludingInterfaces(List())(className)
        val compRes = classLines.toSet intersect superClassStrs.toSet
       if(! compRes.isEmpty) { 
         
         
         val methDefList = clsDef.methods
         val methNameANdDefs = methDefList map ( (mdef) => {
           val methName = StringUtils.getMethNameFromMethPath( mdef.methodPath)
          
           (methName, mdef)
         })
        
        
         // we are going to filter any override methods as entries
         val filteredMethNameAndMDefs = methNameANdDefs filter {
           case (mn, mdef) => {
             val  relativeMethName =  StringUtils.getMethNameFromMethPath(mn)
             
             
             val res = (entries.contains(mn) && 
             (!mdef.attrs.contains("abstract")) &&
             (!mdef.attrs.contains("private")) ) || 
             (xmlEntries.contains(relativeMethName) && 
             (!mdef.attrs.contains("abstract")) &&
             (!mdef.attrs.contains("private")) ) 
             
             // here is a convinient to set the entry point flag
             if(res) mdef.isEntryPoint = true else false
             res
           }
         }
         
         val entryPointList = filteredMethNameAndMDefs.foldLeft(List[EntryPoint]())((resi, p) => {
           val (mn, md ) = p
           //little tset
          
           EntryPoint(md.methodPath, md.argTypeList, md.body, md.regsNum) :: resi
         })
         
         (clsDef, entryPointList) :: res
        
       } else res
        }
      }
    }
  }
  
  
  /**
   * search all the entrypoints in the current apps!
   * 
   */
  private def getRawEntryPoints: List[(DalvikClassDef, List[EntryPoint])] ={
    //let's first just return the dumb one for now
   // val clsDefO = DalvikClassDef.forName("org/ucomb/tests/TestExnssActivity")
    //val clsDefO = DalvikClassDef.forName("org/ucomb/AndroidDacapoActivity")
    // val clsDefO = DalvikClassDef.forName("org/ucombinator/tests/TestStringsActivity") 
    //val clsDefO=  DalvikClassDef.forName("org/ucomb/Interface3Activity")
   //  val clsDefO=  DalvikClassDef.forName("org/ucomb/android/testinterface/InterfaceTest2Activity")
   // val clsDefO=  DalvikClassDef.forName("org/ucomb/TestInterfaceAndroid1Activity")
    //val clsDefO=  DalvikClassDef.forName("org/ucomb/TestGCSimActivity")
    //val clsDefO=  DalvikClassDef.forName("com/ultracoolmap/UltraCoolMapActivity") 
    val clsDefO=  DalvikClassDef.forName("org/ucomb/AndroidAntlrActivity")  
    val clsDef = clsDefO match {
      	case Some(s) => s
      	case None => throw new Exception("the specified class not found")
    }
     //val md  = DalvikClassDef.lookupMethod("org/ucomb/TestInterfaceAndroid1Activity", "org/ucomb/TestInterfaceAndroid1Activity/onCreate", List("(object android/os/Bundle)"), 1)
  //   val md  = DalvikClassDef.lookupMethod("org/ucomb/Interface3Activity", "org/ucomb/Interface3Activity/onCreate", List("(object android/os/Bundle)"), 1)
   //   val md  = DalvikClassDef.lookupMethod("org/ucomb/android/testinterface/InterfaceTest2Activity", "org/ucomb/android/testinterface/InterfaceTest2Activity/onCreate", List("(object android/os/Bundle)"), 1)
//    val md  = DalvikClassDef.lookupMethod("org/ucomb/tests/TestExnssActivity", "org/ucomb/tests/TestExnssActivity/onCreate", List("(object android/os/Bundle)"), 1)
   // val md  = DalvikClassDef.lookupMethod("org/ucomb/AndroidDacapoActivity", "org/ucomb/AndroidDacapoActivity/onCreate", List("(object android/os/Bundle)"), true)
   // val md  = DalvikClassDef.lookupMethod("org/ucombinator/tests/TestStringsActivity", "org/ucombinator/tests/TestStringsActivity/onCreate", List("(object android/os/Bundle)"), true)
 //  val md  = DalvikClassDef.lookupMethod("org/ucomb/TestGCSimActivity", "org/ucomb/TestGCSimActivity/onCreate", List("(object android/os/Bundle)"), 1)
  //  val md  = DalvikClassDef.lookupMethod( "com/ultracoolmap/UltraCoolMapActivity", "com/ultracoolmap/UltraCoolMapActivity/onCreate" , List("(object android/os/Bundle)"), 1)
    val md  = DalvikClassDef.lookupMethod( "org/ucomb/AndroidAntlrActivity", "org/ucomb/AndroidAntlrActivity/onCreate" , List("(object android/os/Bundle)"), 1)
    Debug.prntDebugInfo("md", md)
    val ens = List(EntryPoint(md.head.methodPath, md.head.argTypeList, md.head.body, md.head.regsNum))
    List((clsDef, ens))
  }
  
 
  private def buildEntryPointInvokeStmts(objRegStr: String, ens: List[EntryPoint], clP: String): List[EntryPointInvokeStmt] ={
    /**
     * TODO maybe nwe need exact clsPATH, methpath
     */
   ens.map((en) => {EntryPointInvokeStmt(en, objRegStr, StmtNil, StmtNil, clP, en.methodPath)})
  }
  
  //found the init with the maixnum number of arguments
  // return it and the rest initentrypoints
  private def buildInitEntries(initDefs: List[MethodDef], clsPath:String) : (Stmt, List[Stmt]) = {
      val (maxInit, restI ) = findMaxArgsInit(initDefs)
       val methP = ""
      val initEntryStmt = InitEntryPointStmt(maxInit.methodPath, maxInit.argTypeList, maxInit.body, maxInit.regsNum, StmtNil, StmtNil,clsPath, methP)
      val hdInit = CommonUtils.randomizeLineNumberOneStmt(initEntryStmt, clsPath, methP)
   
        
    val restInitSts = 
    restI.foldLeft(List[Stmt]())((res, initD) => {
      val thisRegExp = CommonUtils.getThisRegStr(initD.regsNum,initD.argTypeList.length)
      /**
       * ToDo fix teh methPath
       */
      val methP = ""
      val initEntryStmt = InitEntryPointStmt(initD.methodPath, initD.argTypeList, initD.body, initD.regsNum, StmtNil, StmtNil,clsPath, methP)
      val initEntryStmtWithRandomLineNo = CommonUtils.randomizeLineNumberOneStmt(initEntryStmt, clsPath, methP)
      res :::List(initEntryStmtWithRandomLineNo)  
    })
   ( hdInit, restInitSts)
  }

  private def findMaxArgsInit(initDefs: List[MethodDef]): (MethodDef, List[MethodDef]) = {

    if (initDefs.length == 1) (initDefs.head, initDefs)
    else {

      var max = 0
      var idex = 0
      initDefs.foreach((id: MethodDef) => {
        val tempL = id.argTypeList.length
        if (tempL > max) {
          max = tempL
          idex = initDefs.indexOf(id)
        } else {}
      })

      val id = initDefs(idex)
      val restt = initDefs diff List(id)
      (id, restt)
    }
  }
  
  // return the rest of the init entry ppints and the chained togethered init paths with its entrypoints
  //  linked them together
  private def allInitWithfollowingEntries (initDefs: List[MethodDef], ens: List[EntryPoint], clsP:String) : (List[Stmt], List[Stmt]) = {
    val (hdInit, restInits) = buildInitEntries(initDefs,clsP)
    //randomly get one init and intialize the reg
    val initD = hdInit.asInstanceOf[InitEntryPointStmt]
    val thisRegExp = CommonUtils.getThisRegStr(initD.regsNum,initD.argsTypes.length)
     val entryStmts = buildEntryPointInvokeStmts(thisRegExp,ens, clsP)
       val linkHeadO= 
     CommonUtils.linkedListWrapper(List())(hdInit :: entryStmts) 
     
     val initPaths = 
     linkHeadO match {
      case Some(hi) => List(hi)
      case None => List()
    }
     	 (restInits, initPaths)
  }
  
  
  private def copeInitEntryStmt(ie: InitEntryPointStmt): InitEntryPointStmt = {
      InitEntryPointStmt(ie.methodPath , ie.argsTypes , ie.body , ie.regsNum , ie.nxt , ie.ln , ie.clsP , ie.methP)
  }
  //eachInitEntryPairs, inddividual inits
    private def allInitAndWithfollowingInitEntries (initDefs: List[MethodDef], ens: List[EntryPoint], clsP:String) : (List[Stmt], List[Stmt]) = {
    val (hdInit, restInits) = buildInitEntries(initDefs,clsP)
    //randomly get one init and intialize the reg
    val initD = hdInit.asInstanceOf[InitEntryPointStmt]
    val thisRegExp = CommonUtils.getThisRegStr(initD.regsNum,initD.argsTypes.length)
     val entryStmts = buildEntryPointInvokeStmts(thisRegExp,ens, clsP)
      
   
   
   // init-entry, inii-tentry
   val initCopies = entryStmts.map((en)=> {copeInitEntryStmt(initD)})
   val initEnPairs = initCopies.zip(entryStmts)
    /**
     * just chain every init with single entry
     * and then get all the init entry chian
     */
    val eachInitEntryPairs= 
    initEnPairs.foldLeft(List[Stmt]())((res, inen) => {
   
       val linkHeadO= 
    	   CommonUtils.linkedListWrapper(List())(inen._1 :: List(inen._2))  
      
     linkHeadO match {
      case Some(hi) => res ::: List(hi)
      case None => res
      
    }
    }) 
    
       /* val linkHeadO= 
    	   CommonUtils.linkedListWrapper(List())(initD ::  entryStmts)  
      
      val eachInitEntryPairs = 
    
      
     linkHeadO match {
      case Some(hi) =>  List(hi)
      case None => List()
    }*/
     	 (eachInitEntryPairs, restInits)
  }
  
  
  /**
   * This can make you dead slow
   * because for each (random) init + all the entries? seriously?
   * */
  private def getOneExecutionInitEntryPath(initDefs: List[MethodDef], ens: List[EntryPoint], clsP: String) : List[Stmt]={
    initDefs.foldLeft(List[Stmt]())((res, initD) => {
      val thisRegExp = CommonUtils.getThisRegStr(initD.regsNum,initD.argTypeList.length)
      val initEntryStmt = InitEntryPointStmt(initD.methodPath, initD.argTypeList, initD.body, initD.regsNum, StmtNil, StmtNil, clsP,initD.methodPath)
      val initEntryStmtWithRandomLineNo = CommonUtils.randomizeLineNumberOneStmt(initEntryStmt, clsP, initD.methodPath)
      Debug.prntDebugInfo("the init entry line no is:  ", initEntryStmtWithRandomLineNo.lineNumber) 
      val entryStmts = buildEntryPointInvokeStmts(thisRegExp,ens, clsP)
      Debug.prntDebugInfo("the entryStmts of the init:  "+ initD.methodPath, entryStmts) 
      res ::: List(initEntryStmtWithRandomLineNo) ::: entryStmts
    })
  }
  
  /**
   * What about let's just get the onCretae the first?
   * For service, not resolved onStart first or onCreate. But let's get the onCreate first?
   * It should be only onCreate method since all the compoenet are class and java is single inheritance
   */
  private def orderEntries(ens: List[EntryPoint]) : List[EntryPoint] = {
    val anyONCreates = ens.filter((en) => {
      en match {
        case esi@EntryPoint(methodPath ,  argTypes , body , regsNum  ) => {
          
          esi.methodPath.contains("onCreate")
        }
        case _ => false
       
      }
    })
   
    val exactOnCreates = anyONCreates.filter(_.methodPath.endsWith("onCreate"))
    val notExactOnCretae = anyONCreates  diff exactOnCreates 
    val restElems = ens  diff anyONCreates 
    exactOnCreates ::: notExactOnCretae ::: restElems
   
  }
  
  private def getEntryPointStmts(opts: AIOptions): List[(List[Stmt], List[Stmt])] ={
    
    val listEns = extractRawEntryPoints(opts)  // getRawEntryPoints
    
     
    if(listEns.isEmpty){
      Debug.prntDebugInfo("No cls + entry points found ", "")
      List()
    }else{ 
      
     listEns.foldLeft(List[(List[Stmt], List[Stmt])]())((res, clsEns)=>{
       val (clsDef, ens) = clsEns
       if(ens.isEmpty){
            Debug.prntDebugInfo("No entry points found  for the app impossible!", listEns)
            res
       }
       else {
         val initDefs = clsDef.getInitMethods
         //println("initDefs:sdfdsfds=======")
         //initDefs.foreach(println)
         if(initDefs.isEmpty) res 
         else{
         // here the init is also condiered part of entry points
         // so we set the flag in the entry init
         initDefs.foreach(_.isEntryPoint = true)
         val orderedEntries = orderEntries(ens)
         orderedEntries.foreach(println)
         val allInitPaths = allInitAndWithfollowingInitEntries(initDefs, orderedEntries, clsDef.clsPath)
         // getOneExecutionInitEntryPath(initDefs, ens)
        
      //   val linkHeadO= 
        //	 CommonUtils.linkedListWrapper(List())(allInitPaths) 
         res ::: List(allInitPaths)
       }
       }
     })
    }
  }
  
  // returns a list of 
  // init entry path and the rest inits 
  def getLinkedEntryPointHead(opts: AIOptions): (List[Stmt], List[Stmt]) = {
    val  entryPointStmts = getEntryPointStmts(opts)
    
    val allIndividualInits = entryPointStmts.flatten{
      case (initEntrySts, inits) => {
        inits
      }
    }
    
    // chained the initentry pointsstatemetn and its body statements
    val allInitsWithItsBodies = allIndividualInits.flatten {
      case ies@InitEntryPointStmt(_, _, _, _, _, _, _, _) => {
        val inbd = ies.body
        val bdSts = CommonUtils.flattenLinkedStmt(List())(inbd)
        ies :: bdSts
      }
    }
    
    // initEntries inlcude the initentrystmt and the entrypoints
    // they are going to be as the starting point to be explored.
    val initEntries = entryPointStmts.flatten{
      case (initEntrySts, inits ) => initEntrySts
    } 
    
  /*  println("To explore the entry:in get links INit----- " + initEntries.length )
    initEntries.foreach( (entryStmt) => {
     CommonUtils.flattenLinkedStmt(List())(entryStmt).foreach(println)})
    */
     // here you are not going to link all the init entries.
  //  (initEntries, allIndividualInits) 
     (initEntries, allIndividualInits) 
    
   /* val linkHeadO= 
     CommonUtils.linkedListWrapper(List())(initEntries) 
    val linkHead = 
     linkHeadO match {
      case Some(s) => s
      case None => { StmtNil }
    }
     val flatlist = CommonUtils.flattenLinkedStmt(List())(linkHead)
  
   (linkHead, allInitsWithItsBodies)*/
  }
  
 
  
  
  
  
  
 
}