package hu.uw.pallergabor.dedexer;

public class SExpHelpers {
  

  /* MBM: Converts Java type to SX type */
  public static final String LTypeToSXType(String javaType) {
    String type = javaType ;
    
    if (javaType.equals("I"))
      type = "int";
    
    else if (javaType.equals("J"))
      type = "long";    
    
    else if (javaType.equals("B"))
      type = "byte";
    
    else if (javaType.equals("Z"))
      type = "boolean";
    
    else if (javaType.equals("C"))
      type = "char"; 
    
    else if (javaType.equals("D"))
      type = "double";
    
    else if (javaType.equals("F"))
      type = "float";
    
    else if (javaType.equals("S"))
      type = "short";
    
    else if (javaType.equals("V"))
      type = "void";
    
    else if (javaType.startsWith("L"))
      type = "[object " + javaType.substring(1,javaType.length()-1) + "]" ;
    // suposed to be the normal instance not starting with L, so no need to do any change(?)
    else if (javaType.startsWith("[object"))
    	 type =  javaType ;
    
    else if (javaType.startsWith("[")) {
      type = "[array " + LTypeToSXType(javaType.substring(1)) + "]" ;}
    	
    
    
    return type ;
  } 

}
