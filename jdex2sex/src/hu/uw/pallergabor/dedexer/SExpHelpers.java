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
    
    else if (javaType.startsWith("L")) {
    	String[] splited = javaType.split(" ");
    	if(splited.length >= 2) {
    		 type =  javaType ;
    	}
    	else {
    	type = "[object " + javaType.substring(1,javaType.length()-1) + "]" ;}
//    	if(javaType.length() > 2 && javaType.charAt(1) != 'L') {
//    		 type = "[object " + javaType + "]" ;
//    	}
//    		 else {
//    			 type = "[object " + javaType.substring(1,javaType.length()-1) + "]" ;
//    		 }
    }
    
    // suposed to be the normal instance not starting with L, so no need to do any change(?)
    else if (javaType.startsWith("[object")) {
    	 type =  javaType ;
    }
    	
    
    else if (javaType.startsWith("[")) {
      type = "[array " + LTypeToSXType(javaType.substring(1)) + "]" ;}
    
    return type ;
  } 


  public static final String LTypeToSXTypes(String javaTypes) {
    StringBuilder sb = new StringBuilder();
    while (!javaTypes.isEmpty()) {
      int endIndex = endIndexOfFirstType(javaTypes);

      String javaType = javaTypes.substring(0, endIndex);
      javaTypes = javaTypes.substring(endIndex);

      sb.append(LTypeToSXType(javaType));
      sb.append(" ");
    }
    return sb.toString();
  }
  
  private static int endIndexOfFirstType(String javaTypes) {
	int i = 1;
    if (javaTypes.charAt(0) == '[')
      i += endIndexOfFirstType(javaTypes.substring(1));
    else if (javaTypes.charAt(0) == 'L')
      i = javaTypes.indexOf(';') + 1;
    return i;
  }

}
