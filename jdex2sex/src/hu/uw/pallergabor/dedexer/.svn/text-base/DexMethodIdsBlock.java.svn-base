/**
  * Parses the method id table
  */

package hu.uw.pallergabor.dedexer;
import java.io.IOException;

public class DexMethodIdsBlock extends DexParser {

    public void parse() throws IOException {
        int methodsSize = (int)dexPointerBlock.getMethodIdsSize();
        file.seek( dexPointerBlock.getMethodIdsOffset() );
        methods = new String[ methodsSize ];
        protos = new String[ methodsSize ];
        sprotos = new String[ methodsSize ];
        for( int i = 0 ; i < methodsSize ; ++i ) {
            int classTypeIdx = read16Bit();
            int protoIdx = read16Bit();
            int nameStringIdx = (int)read32Bit();
            StringBuilder b = new StringBuilder();
// Class name
            String clazzName = SExpHelpers.LTypeToSXType(dexTypeIdsBlock.getClassName( classTypeIdx ));	
            // 
            if (clazzName.endsWith(";")) { //this is not enough to escape class method!
            	//clazzName = clazzName.substring(0, clazzName.length() - 1);
            	clazzName = SExpHelpers.LTypeToSXType(clazzName);
            }
            b.append( clazzName ); 
            
            b.append( '/' );
// Field name
            //System.out.println(dexStringIdsBlock.getString( nameStringIdx ) );
            b.append( dexStringIdsBlock.getString( nameStringIdx ) );
            //System.out.println(new String(b));
            methods[i] = new String( b );

// Proto
            b = new StringBuilder();
            b.append( dexStringIdsBlock.getString( nameStringIdx ) );
            b.append( '(' );
            b.append( dexProtoIdsBlock.getParameterValueTypes( protoIdx ) );
            b.append( ')' );
            b.append( dexProtoIdsBlock.getReturnValueType( protoIdx ) );
            protos[i] = new String( b );
            dump( "method["+i+"]: "+methods[i]+" ("+protos[i]+")" );
// S-Expression protos
            b = new StringBuilder();
            b.append( dexStringIdsBlock.getString( nameStringIdx ) );
            b.append( '(' );
            b.append( dexProtoIdsBlock.getParameterValueSTypes( protoIdx ) );
            b.append( ')' );
            b.append( SExpHelpers.LTypeToSXType( dexProtoIdsBlock.getReturnValueType( protoIdx ) ) );
            sprotos[i] = new String( b );
            b = new StringBuilder();
        }
    }

    public int getMethodsSize() {
        return (int)dexPointerBlock.getMethodIdsSize();
    }

    public String getMethod( int idx ) {
        return methods[ idx ];
    }

    public String getProto( int idx ) {
        return protos[ idx ];
    }

    public String getSProto( int idx ) {
        return sprotos[ idx ];
    }

    public static String getMethodName( String fullMethodName ) {
        int idx = fullMethodName.lastIndexOf( '/' );
        if( idx < 0 )
            return fullMethodName;
        return fullMethodName.substring( idx+1 );
    }

    public static String combineMethodNameAndProto( String methodName, String proto ) {
        String combined = proto;
        int pidx = methodName.indexOf( '/' );
        if( pidx >= 0 )
            combined = methodName.substring( 0,pidx+1 )+proto;
        return combined;
    }

/**
  * Returns the result type of a method, given its prototype
  * @param prototype of the method.
  * @return The result type ("V" if void)
  */
    public static String getResultType( String proto ) {
        int pos = proto.lastIndexOf( ')' );
        if( pos < 0 )
            return null;
        return proto.substring( pos+1 );
    }

    public void setDexPointerBlock( DexPointerBlock dexPointerBlock ) {
        this.dexPointerBlock = dexPointerBlock;
    }

    public void setDexStringIdsBlock( DexStringIdsBlock dexStringIdsBlock ) {
        this.dexStringIdsBlock = dexStringIdsBlock;
    }

    public void setDexTypeIdsBlock( DexTypeIdsBlock dexTypeIdsBlock ) {
        this.dexTypeIdsBlock = dexTypeIdsBlock;
    }

    public void setDexProtoIdsBlock( DexProtoIdsBlock dexProtoIdsBlock ) {
        this.dexProtoIdsBlock = dexProtoIdsBlock;
    }

    private String              methods[];
    private String              protos[];
    private String              sprotos[];
    private DexPointerBlock     dexPointerBlock = null;
    private DexStringIdsBlock   dexStringIdsBlock = null;
    private DexTypeIdsBlock     dexTypeIdsBlock = null;
    private DexProtoIdsBlock    dexProtoIdsBlock = null;
}
