package hu.uw.pallergabor.dedexer;

/**
  * This wrapper class is used by DexEncodedArrayParser. This object is a Character
  * which formats itself appropriately when its toString method is called.
  */
public class StaticCharacter {
		public StaticCharacter( char c ) {
			instance = new Character( c );
		}

		public String toString() {
	/*		return 	"'"+
			+ DexStringIdsBlock.escapeString( instance.toString() )+
				"'";*/
			
			String ss = instance.toString();
			if(ss.length() == 1){
				if(ss.charAt(0) == '\"'){
				return "#\\\"";
				}else 
					return 	//"'"+
					"#\\" // transformed to racket char format
				+ DexStringIdsBlock.escapeString( ss) ;//+
			} else{
				return 	//"'"+
				"#\\" // transformed to racket char format
			+ DexStringIdsBlock.escapeString( ss) ;//+
				//"'";
			}
			
		}

		private Character instance;
}
