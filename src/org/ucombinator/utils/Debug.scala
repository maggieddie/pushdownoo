package org.ucombinator.utils
import org.ucombinator.dalvik.syntax.SName

object Debug {
  
 

  def prntDebugInfo(context: String, value: Any) {
    if(AIOptions.debugInfo ){
    		System.out.println("Context: " + context +  ":::: " + "Val is: " + value)
    }
    		
  }
  
  def prntErrInfo(context: String, expected: String, found: Any) {
    System.err.println("Error Context: " + context + "\n" + "Expected: " + expected + "," + "found: " + found)
  }
  

}