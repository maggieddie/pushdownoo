package org.ucombinator.dalvik.cfa.cesk
import org.ucombinator.dalvik.syntax.StForEqual
import org.ucombinator.dalvik.syntax.Stmt
import org.ucombinator.dalvik.syntax.StmtNil

trait StmtForEqual {

   def buildStForEqual(st: Stmt): StForEqual ={
    st match {
      case StmtNil => StForEqual(StmtNil, StmtNil, "", "",  StmtNil)
      case _ => StForEqual(st, st.next, st.clsPath, st.methPath, st.lineNumber)
    }
  }
}