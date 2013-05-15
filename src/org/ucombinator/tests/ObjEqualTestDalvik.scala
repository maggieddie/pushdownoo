package org.ucombinator.tests
import org.ucombinator.dalvik.cfa.cesk.StateSpace
import org.ucombinator.dalvik.syntax._


object ObjEqualTest {

  type Time = List[Stmt]
  
   sealed trait Pointer { //extends Ordered[Pointer]{
    // def compare (that: Pointer) 
    // for any given pointer, we would like to converting offset into the 
    // address
    def offset (name: String) : Addr
  }
  case class FramePointer(t: Time, str: String) extends Pointer {
    def offset(regName: String) : RegAddr = new RegAddr(this, regName)
    // don't need
    def push(t: Time, invkSt: Stmt): FramePointer = new FramePointer(t, str)
    
    override def hashCode = 41 * super.hashCode + t.hashCode() + str.hashCode()
    override def equals(other: Any) = other match {
      case that : FramePointer => (that canEqual this) && 
      								(this.t == that.t) && (this.str == that.str)
      case _ => false
    }
    override def canEqual(other: Any) = other.isInstanceOf[FramePointer]
    
  }
  case class ObjectPointer(t: Time, newS: NewStmt, clsName: String) extends Pointer {
    def offset(fieldName: String): FieldAddr = new FieldAddr(this, fieldName)
    def push(t: Time, ns: NewStmt, clsName: String): ObjectPointer = new ObjectPointer(t, newS, clsName)

    override def hashCode = 41 * super.hashCode + t.hashCode() + newS.hashCode() + clsName.hashCode()
    override def equals(other: Any) = other match {
      case that: ObjectPointer => (that canEqual this) &&
        (this.t == that.t) && (this.newS == that.newS) && (this.clsName == that.clsName)
      case _ => false
    }
    override def canEqual(other: Any) = other.isInstanceOf[ObjectPointer]
  }
  
  /**********************************************
   * Various kinds of Addr in Dalvik analysis
   * 1.  RegAddr = FramePointer * RegName
   * 2.  FieldAddr = ObjectPointer * FieldName
   * 3.  KontAddr = not Known yet ...
   ********************************************/
  sealed abstract class Addr //extends some Ordering 
  
  abstract class OffsetAddr extends Addr {
    def pointer : Pointer
    def offset : String
    
     override def hashCode = 37 * super.hashCode + pointer.hashCode() + offset.hashCode() 
    override def equals(other: Any) = other match {
      case that: OffsetAddr => //(that canEqual this) && 
      (that.pointer == pointer) &&
        (this.pointer == that.pointer) && (this.offset == that.offset) 
      case _ => false
    }
   // override def canEqual (other:Any) = other.isInstanceOf[OffsetAddr]
  
  }
  
  case class KontAddr() extends Addr
  case class RegAddr(fp: FramePointer, offs: String) extends OffsetAddr {
    def pointer = fp
    def offset = offs
    
    override def hashCode = 41 * super.hashCode + pointer.hashCode() + offset.hashCode()
    override def equals(other: Any) = other match {
      case that: RegAddr => (that canEqual this) && super.equals(that)
      case _ => false
    }
    override def canEqual(other: Any) = other.isInstanceOf[RegAddr]
  }

  case class FieldAddr(op: ObjectPointer, field: String) extends OffsetAddr {
    def pointer = op
    def offset = field
    
    override def hashCode = 37 * super.hashCode + pointer.hashCode() + offset.hashCode()
    override def equals(other: Any) = other match {
      case that: RegAddr => (that canEqual this) && super.equals(that)
      case _ => false
    }
    override def canEqual(other: Any) = other.isInstanceOf[FieldAddr]
  }
  // ..
  
  def main(args: Array[String]): Unit = {
    // test the equality of the all the address! 
    
      val map = Map.empty[Addr, Set[Int]]
      
     // case class InvokeStmt(methPathStr: String, argRegAExp: List[AExp], objAExp: AExp, tyStrs: List[String], nxt: Stmt)
      val invS = new InvokeStmt("callMethod", List(RegisterExp(SName.from("v1"))), RegisterExp(SName.from("v1")), List("Boolean"), GotoStmt("sdf", StmtNil, StmtNil, "", ""), StmtNil, "", "")
      val invS2 = new InvokeStmt("callMethod", List(RegisterExp(SName.from("v1"))), RegisterExp(SName.from("v1")), List("Boolean"), GotoStmt("sdf", StmtNil, StmtNil , "", ""), StmtNil, "", "")
      val fp1 = new FramePointer(List(), "com/android/demo/notepad3/NoteEdit/factorial") //GotoStmt("lsf", StmtNil))
      val regAddr = fp1.offset("v2")
      val map2 = map + (regAddr -> Set(100))
      
      val fp11 = new FramePointer(List(), "com/android/demo/notepad3/NoteEdit/factorial") //GotoStmt("lsf", StmtNil))
      
      val fp2 =  FramePointer(List(), "com/android/demo/notepad3/NoteEdit/factorial") 
      val regAddr2 = fp2.offset("v2")
       println(map2 get regAddr)
      println(map2 get regAddr2)
      
      
      
    //  val op1 = new ObjectFrame(ObjectPointer)
      
          
    
  }

}