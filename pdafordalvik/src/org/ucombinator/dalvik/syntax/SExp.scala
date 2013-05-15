/*
 * CRAPL 2012.
 * U Combinator, University of Utah
 * DistriNet, KU Leuven
 *
 * THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
 * APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
 * HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT
 * WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND
 * PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE
 * DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
 * CORRECTION.
 *
 * IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
 * WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR
 * CONVEYS THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
 * INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT
 * NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR
 * LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM
 * TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER
 * PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
 *
 * If you have questions or concerns about the CRAPL, or you need more
 * information about this license, please contact:
 *
 *    Matthew Might
 *    http://matt.might.net/
 */

package org.ucombinator.dalvik.syntax

import util.parsing.input.Positional
import org.ucombinator.dalvik.parsing.SExpParser

object SExp {

  /**
  Determines the style with which names are printed.
   */
  var shouldNamesBeSymbols = true

  def apply(list: List[SExp]): SExp = list match {
    case hd :: tl => :+:(hd, apply(tl))
    case Nil => SNil()
  }

  def apply(list: List[SExp], tombstone: SExp): SExp = list match {
    case hd :: tl => :+:(hd, apply(tl, tombstone))
    case Nil => tombstone
  }

  def parseAllIn(filename: String): List[SExp] = {
    val input = scala.io.Source.fromFile(filename).mkString("")
    parseAll(input)
  }

  def parseAll(input: String): List[SExp] = {
    val p = new SExpParser
    p.parseAll(input)
  }

  def parse(input: String): SExp = {
    val p = new SExpParser
    p.parse(input)
  }

  private var maxSerialNumber = 0

  def allocateSerialNumber(): Long = {
    maxSerialNumber += 1
    maxSerialNumber
  }
}

/* S-Expressions. */



abstract class SExp extends Positional {
  lazy val serialNumber: Long = SExp.allocateSerialNumber()

  def toString: String;

  def toList: List[SExp];

  def toDottedList: (List[SExp], SExp);

  def isKeyword: Boolean;

  def isInteger: Boolean;

  def isList: Boolean;

  def isPair: Boolean;

  def isNull: Boolean;

  def isSymbol: Boolean;

  def isName: Boolean;
}


final case class SInt(val value: BigInt) extends SExp {
  override def toString = value.toString

  def toList = throw new Exception("Cannot convert integer to list.")

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = true

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false
}


final case class SChar(val value: Char) extends SExp {
  override def toString = "#\\" + value.toString

  def toList = throw new Exception("Cannot convert integer to list.")

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = false

  def isChar = true

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false
}


final case class SText(val value: String) extends SExp {
  // TODO/FIXME: Escape the string value
  override def toString = "\"" + value + "\""

  def toList = throw new Exception("Cannot convert string to list.")

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = false

  def isString = true

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false
}


case class SBoolean(val value: Boolean) extends SExp {
  override def toString = (if (value) {
    "#t"
  } else {
    "#f"
  })

  def toList = throw new Exception("Cannot convert Boolean to list.")

  def toDottedList = (List(), this)

  def isFalse = value

  def isKeyword = false

  def isInteger = false

  def isString = false

  def isBoolean = true

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false
}


case class SKeyword(val string: String) extends SExp with Ordered[SKeyword] {
  override def toString = "#:" + string

  def toList = throw new Exception("Cannot convert keyword to list.")

  def toDottedList = (List(), this)

  def isFalse = false

  def isKeyword = true

  def isInteger = false

  def isString = false

  def isBoolean = false

  def isList = false

  def isPair = false

  def isNull = false

  def isName = false

  def isSymbol = false

  def compare(that: SKeyword) = this.string compare that.string
}


abstract  class SSymbol(val string: String) extends SExp {
}


final case class SName(s: String, version: Int) extends SSymbol(s) with Ordered[SName] {
  def compare(that: SName): Int = that match {
    case SName(s2, v2) => {
      val cmpString = s compare s2
      if (cmpString != 0)
        cmpString
      else
        version compare v2
    }
  }

  override def toString =
    if (version == 0) {
      string
    } else {
      if (SExp.shouldNamesBeSymbols)
        s + "$" + version
      else
        "#name[" + string + " " + version + "]"
    }

  def toList = throw new Exception("Cannot convert symbol to list.")

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = false

  def isList = false

  def isPair = false

  def isNull = false

  def isSymbol = true

  def isName = true

  override def hashCode: Int = s.hashCode() * 10 + version

  override def equals(a: Any) = a match {
    case SName(s2, v2) => {
    (s equals s2) && (version == v2)
    }
    case _ => false
  }
}

final case class SNil() extends SExp {
  override def toString = "()"

  def toList = List()

  def toDottedList = (List(), this)

  def isKeyword = false

  def isInteger = false

  def isList = true

  def isPair = false

  def isNull = true

  def isName = false

  def isSymbol = false
}

final case class :+:(var car: SExp, var cdr: SExp) extends SExp {
  
  override def toString = this.toDottedList match {
    case (l, SNil()) => "(" + (l mkString " ") + ")"
    case (l, end) => "(" + ((l mkString " ") + " . " + end) + ")"
  }

  def toList = car :: cdr.toList

  def toDottedList: (List[SExp], SExp) = {
    val (lst, end) = cdr.toDottedList
    return (car :: lst, end)
  }

  def isKeyword = false

  def isInteger = false

  def isList = cdr.isList

  def isPair = true

  def isNull = false

  def isName = false

  def isSymbol = false
}

object SList {
  def apply(sx: SExp*): SExp =
    SExp(sx.toList)

  def unapplySeq(sx: SExp): Option[List[SExp]] = {
    if (sx isList)
      Some(sx toList)
    else
      None
  }
}


object SKeyword {

  private val keywordTable = scala.collection.mutable.HashMap[String, SKeyword]()

  def from(string: String): SKeyword = {
    (keywordTable get string) match {
      case Some(kw) => kw
      case None => {
        val kw = SKeyword(string)
        keywordTable(string) = kw
        kw
      }
    }
  }
}


object SName {

  private val nameTable = scala.collection.mutable.HashMap[String, SName]()
  private val maxTable = scala.collection.mutable.HashMap[String, SName]()

  def from(string: String): SName = {
    (nameTable get string) match {
      case Some(name) => name
      case None => {
        val name = SName(string, 0)
        nameTable(string) = name
        name
      }
    }
  }

  def from(symbol: SSymbol): SName = {
    from(symbol.string)
  }

  def gensym(string: String): SName = {
    (maxTable get string) match {
      case Some(SName(_, v)) => {
        val name = SName(string, v + 1)
        maxTable(string) = name
        name
      }
      case None => {
        val name = SName(string, 1)
        maxTable(string) = name
        name
      }
    }
  }

  def gensym(symbol: SSymbol): SName = {
    gensym(symbol.string)
  }
}


object CommonSSymbols {

  val SQuote = SName.from("quote")
  val SQuasiquote = SName.from("quasiquote")
  val SUnquote = SName.from("unquote")
  val SUnquoteSplicing = SName.from("unquote-splicing")

  val SCons = SName.from("cons")
  val SListSym = SName.from("list")
  val SAppend = SName.from("append")
  val SCar = SName.from("car")
  val SCdr = SName.from("cdr")
  val STypeP = SName.from("type?")

  val SDefine = SName.from("define")

  val SDefineStruct = SName.from("define-struct")
  val SMakeStruct = SName.from("make-struct")
  val SStructGet = SName.from("struct-get")
  val SStructSet = SName.from("struct-set!")

  val SLambda = SName.from("lambda")

  val SLet = SName.from("let")
  val SLetStar = SName.from("let*")
  val SLetRec = SName.from("letrec")

  val SSetBang = SName.from("set!")
  val SBegin = SName.from("begin")
  val SVoid = SName.from("void")

  val SIf = SName.from("if")
  val SCond = SName.from("cond")
  val SCase = SName.from("cond")
  val SElse = SName.from("else")
  val SRightArrow = SName.from("=>")
  val SAnd = SName.from("and")
  val SOr = SName.from("or")

  val SValues = SName.from("values")
  val SLetValues = SName.from("let-values")
  
  
  // those added for Dalvik for test
  val SClass = SName.from("class")
  val SAttrs = SName.from("attrs")
   val SPub = SName.from("public")
   val SPrivate = SName.from("private")
   val SFinal = SName.from ("final")
   val SStatic = SName.from("static")
  val SSuper = SName.from("super")
  
  val SSource =SName.from("source")
  val SImpl = SName.from("implements")
  val SInterface = SName.from("interface")
  val SField = SName.from("field")
  val SxVoid = SName.from("void")
  val SNull  = SName.from("null")
  
  val SMethod = SName.from("method")
  val SLimit = SName.from("limit")
  val SRegs = SName.from("registers")
  val SLine = SName.from("line")
  
  val SLabel = SName.from("label")
  val SCatch = SName.from("catch")
  val SCatchAll = SName.from("catchall")
  val SUsing = SName.from("using")
  val  SFrom = SName.from("from")
  val STo = SName.from("to")
  
  val SAnnotation = SName.from("annotation")
  val SSystemVisibility = SName.from("systemVisibility");
  val SAnnotationValue = SName.from("value");
  
  
  val SNop = SName.from("nop")
  
  val SMove = SName.from("move")
  val SMoveFrom16 =SName.from("move/from16")
   val SMove16 =SName.from("move/16")
   val SMoveWide = SName.from("move-wide")
   val SMoveWideFrom16 = SName.from("move-wide/from16")
   val SMoveWide16 = SName.from("move-wide/16")
   val SMoveObject = SName.from("move-object")
   val SMoveObjectFrom16 = SName.from("move-object/from16")
   val SMoveObject16 = SName.from("move-object/16")
  
   
   val SMoveResult = SName.from("move-result")
   val SMoveResultWide = SName.from("move-resut-wide")
   val SMoveResultObject = SName.from("move-result-object")
   
   val SMoveException = SName.from("move-exception")
   
   val SReturnVoid = SName.from("return-void")
   val SReturn = SName.from("return")
   val SReturnWide = SName.from("return-wide")
   val SReturnObject = SName.from("return-object")
   
   val SConst4 = SName.from("const/4")
   val SConst16 = SName.from("const/16")
   val SConst = SName.from("const")
   val SConstHigh16 = SName.from("const/high16")
   val SConstWide16 = SName.from("const-wide/16")
   val SConstWide32 = SName.from("const-wide/32")
   val SConstWide = SName.from("const-wide")
    val SConstWideHigh16 = SName.from("const-wide/high16")
    
    val SConstString = SName.from("const-string")
    val SConstStringJumbo = SName.from("const-string/jumbo")
    
    val SConstClass  = SName.from ("const-class")
  
    val SCheckCast = SName.from("check-cast")
    
    val SNew = SName.from("new-instance")
    val SInstanceOf = SName.from ("instance-of")
    
    // arrays
    
    val SThrow = SName.from("throw")
    
    val SGoto = SName.from("goto")
    val SGoto16 = SName.from("goto/16")
    val SGoto32 = SName.from("goto/32")
    
    // switch
    
    //
    val SCmplFloat = SName.from("cmpl-float")
    val SCmpgFloat = SName.from("cmpg-float")
    val SCmplDouble = SName.from("cmpl-double")
    val SCmpgDouble= SName.from("cmpg-double")
    val SCompLong = SName.from("cmp-long")
    
    // 
    val SInvokeVirtual = SName.from("invoke-virtual")
    val SInvokeSuper = SName.from("invoke-super")
    val SInvokeDirect = SName.from("invoke-direct")
    val SInvokeStatic = SName.from("invoke-static")
    val SInvokeInterface = SName.from("invoke-interface")
 
    val SInvokeVirtualRange = SName.from("invoke-virtual/range")
    val SInvokeSuperRange = SName.from("invoke-super/range")
    val SInvokeDirectRange = SName.from("invoke-direct/range")
    val SInvokeStaticRange = SName.from("invoke-static/range")
    val SInvokeInterfaceRange = SName.from("invoke-interface/range")
    
    val SIfEq = SName.from("if-eq")
    val SIfNe = SName.from("if-ne")
    val SIfLt = SName.from("if-lt")
    val SIfGe = SName.from("if-ge")
    val SIfGt = SName.from("if-gt")
    val SIfle = SName.from("if-le")
    
    val SIfEqz = SName.from("if-eqz")
    val SIfNez = SName.from("if-nez")
    val SIfLtz = SName.from("if-ltz")
    val SIfGez = SName.from("if-gez")
    val SIfGtz = SName.from("if-gtz")
    val SIfLez = SName.from("if-lez")
    
    //sget/sput
    val SGet = SName.from("sget")
    val SGetWide = SName.from("sget-wide")
    val SGetObject= SName.from("sget-object")
    val SGetBoolean = SName.from("sget-boolean")
    val SGetByte = SName.from("sget-byte")
    val SGetChar = SName.from("sget-char")
    val SGetShort = SName.from("sget-short")
    val SPut = SName.from("sput")
    val SPutWide = SName.from("sput-wide")
    val SPutObject = SName.from("sput-object")
    val SPutBoolean = SName.from("sput-boolean")
    val SPutByte = SName.from("sput-byte")
    val SPutChar = SName.from("sput-char")
    val SPutShort = SName.from("sput-short")
    
    // iget/iput
    val SIGet = SName.from("iget")
    val SIGetWide = SName.from("iget-wide")
    val SIGetObject= SName.from("iget-object")
    val SIGetBoolean = SName.from("iget-boolean")
    val SIGetByte = SName.from("iget-byte")
    val SIGetChar = SName.from("iget-char")
    val SIGetShort = SName.from("iget-short")
    val SIPut = SName.from("iput")
    val SIPutWide = SName.from("iput-wide")
    val SIPutObject = SName.from("iput-object")
    val SIPutBoolean = SName.from("iput-boolean")
    val SIPutByte = SName.from("iput-byte")
    val SIPutChar = SName.from("iput-char")
    val SIPutShort = SName.from("iput-short")
    
    
    // unop
    val SNegInt = SName.from("neg-int")
    val SNotInt = SName.from("not-int")
    val SNegLong = SName.from("neg-long")
    val SNotLong = SName.from("not-long")
    val SNegFloat = SName.from("neg-float")
    val SNegDouble = SName.from("neg-double")
    val SIntToLong = SName.from("int-to-long")
    val SIntToFloat = SName.from("int-to-float")
    val SIntToDouble = SName.from("int-to-doule")
    val SLongToInt = SName.from("long-to-int")
    val SLongToFloat = SName.from("long-to-float")
    val SLongToDouble = SName.from("long-to-double")
    val SFloatToInt = SName.from("float-to-int")
    val SFloatToLong = SName.from("float-to-long")
    val SFloatToDouble = SName.from("float-to-double")
    val SDoubleToInt = SName.from("double-to-int")
    val SDoubleToLong = SName.from("double-to-long")
    val SDoubleToDouble = SName.from("double-to-float")
    val SIntToByte = SName.from("int-to-byte")
    val SIntToChar = SName.from("int-to-char")
    val SIntToShort = SName.from("int-to-short")
    
    //biop
    val SAddInt = SName.from("add-int")
    val SSubInt = SName.from("sub-int")
    val SMulInt = SName.from("mul-int")
    val SDivInt = SName.from("div-int")
    val SRemInt = SName.from("rem-int")
    val SAndInt = SName.from("and-int")
    val SOrInt = SName.from("or-int")
    val SXorInt = SName.from("xor-int")
    val SShlInt = SName.from("shl-int")
    val SShrInt = SName.from("shr-int")
    val SUShrInt = SName.from("ushr-int")
    val SAddLong = SName.from("add-long")
    val SSubLong = SName.from("sub-long")
    val SMulLong = SName.from("mul-long")
    val SDivLong = SName.from("div-long")
    val SRemLong = SName.from("rem-Long")
    val SAndLong = SName.from("and-long")
    val SOrLong = SName.from("or-long")
    val SXorLong = SName.from("xor-long")
    val SShlLong = SName.from("shl-long")
    val SShrLong = SName.from("shr-long")
    val SUShrLong = SName.from("ushr-long")
    val SAddFloat = SName.from("add-float")
    val SSubFloat = SName.from("sub-float")
    val SMulFloat = SName.from("mul-float")
    val SDivFloat = SName.from("div-float")
    val SRemFloat = SName.from("rem-float")
    val SAddDouble = SName.from("add-double")
    val SSubDouble = SName.from("sub-double")
    val SMulDouble = SName.from("mul-double")
    val SDivDouble = SName.from("div-double")
    val SRemDouble = SName.from("rem-double")
    
    //biop/2add2
    val SAddInt2Addr = SName.from("add-int/2addr")
    val SSubInt2Addr = SName.from("sub-int/2addr")
    val SMulInt2Addr = SName.from("mul-int/2addr")
    val SDivInt2Addr = SName.from("div-int/2addr")
    val SRemInt2Addr = SName.from("rem-int/2addr")
    val SAndInt2Addr = SName.from("and-int/2addr")
    val SOrInt2Addr = SName.from("or-int/2addr")
    val SXorInt2Addr = SName.from("xor-int/2addr")
    val SShlInt2Addr= SName.from("shl-int/2addr")
    val SShrInt2Addr = SName.from("shr-int/2addr")
    val SUShrInt2Addr = SName.from("ushr-int/2addr")
    val SAddLong2Addr = SName.from("add-long/2addr")
    val SSubLong2Addr = SName.from("sub-long/2addr")
    val SMulLong2Addr = SName.from("mul-long/2addr")
    val SDivLong2Addr = SName.from("div-long/2addr")
    val SRemLong2Addr = SName.from("rem-Long/2addr")
    val SAndLong2Addr = SName.from("and-long/2addr")
    val SOrLong2Addr = SName.from("or-long/2addr")
    val SXorLong2Addr = SName.from("xor-long/2addr")
    val SShlLong2Addr = SName.from("shl-long/2addr")
    val SShrLong2Addr= SName.from("shr-long/2addr")
    val SUShrLong2Addr = SName.from("ushr-long/2addr")
    val SAddFloat2Addr = SName.from("add-float/2addr")
    val SSubFloat2Addr = SName.from("sub-float/2addr")
    val SMulFloat2Addr = SName.from("mul-float/2addr")
    val SDivFloat2Addr = SName.from("div-float/2addr")
    val SRemFloat2Addr = SName.from("rem-float/2addr")
    val SAddDouble2Addr = SName.from("add-double/2addr")
    val SSubDouble2Addr = SName.from("sub-double/2addr")
    val SMulDouble2Addr = SName.from("mul-double/2addr")
    val SDivDouble2Addr = SName.from("div-double/2addr")
    val SRemDouble2Addr = SName.from("rem-double/2addr")
    
    //biop/lit8
    val SAddIntLit8 = SName.from("add-int/lit8")
    val SRSubIntLit8 = SName.from("rsub-int/lit8")
    val SMulIntLit8 = SName.from("mul-int/lit8")
    val SDivIntLit8 = SName.from("div-int/lit8")
    val SRemIntLit8 = SName.from("rem-int/lit8")
    val SAndIntLit8 = SName.from("and-int/lit8")
    val SOrIntLit8 = SName.from("or-int/lit8")
    val SXorIntLit8 = SName.from("xor-int/lit8")
    val SShlIntLit8 = SName.from("shl-int/lit8")
    val SShrIntLit8 = SName.from("shr-int/lit8")
    val SUShrIntLit8 = SName.from("ushr-int/lit8")
    
    //bioplit16
    val SAddIntLit16 = SName.from("add-int/lit16")
    val SRSubIntLit16 = SName.from("rsub-int/lit16")
    val SMulIntLit16 = SName.from("mul-int/lit16")
    val SDivIntLit16 = SName.from("div-int/lit16")
    val SRemIntLit16 = SName.from("rem-int/lit16")
    val SAndIntLit16 = SName.from("and-int/lit16")
    val SOrIntLit16 = SName.from("or-int/lit16")
    val SXorIntLit16 = SName.from("xor-int/lit16")
    
    // multi-branchs
    val SPackedSwitch = SName.from("packed-switch")
    val SSparseSwitch = SName.from("sparse-switch")
    
    

    
  
}