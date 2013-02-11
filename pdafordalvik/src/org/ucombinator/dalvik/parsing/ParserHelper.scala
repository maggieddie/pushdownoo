package org.ucombinator.dalvik.parsing
import org.ucombinator.utils.AIOptions
import scala.tools.nsc.io.Directory
import org.ucombinator.dalvik.syntax.SExp
import java.io.File
import scala.util.matching.Regex

trait ParserHelper {

    private def simplefunc(pf: tools.nsc.io.File, cnt: Int, opts:AIOptions) {

    val fp = pf.path // path.getAbsolutePath();
    val sexp = SExp.parseAllIn(fp)

    if (opts.verbose) {
      // System.err.println("Input program in S-expression Form:")
      //  System.out.println(sexp)
   //   System.out.println("\n")
    } 

    S2DParser(sexp);

   // if (opts.verbose) {
    //  System.err.println("done parsing file: " + fp)
  //  }

  }  
    
   def parseDalvikSExprs(opts: AIOptions) {
    val dirName = opts.sexprDir

    val sexDir = new Directory(new File(dirName))
    val allFileList = sexDir.deepFiles
     
    val sexpFiles = allFileList.filter((f) => {
      f.name.endsWith(".sxddx")
    })

    var cnt = 0
    sexpFiles.foreach((sf) => {
      cnt += 1
      
    if (opts.verbose) {
      System.err.println(cnt + " Parsing file " + sf)
    }
      simplefunc(sf, cnt, opts)
    })

    if (opts.verbose) {
      System.err.println(" Done passing all the s-exp class files! ")
      System.out.println("\n")
    }
  }
}