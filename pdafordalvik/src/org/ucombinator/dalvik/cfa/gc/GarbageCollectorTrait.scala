package org.ucombinator.dalvik.cfa.gc


trait GarbageCollectorTrait  {

  type Addr
  
  type ControlState
  
  type Kont

  def getRootAddrs(c: ControlState, frames: Kont): Set[Addr]

  def gc(c: ControlState, frames: Kont): ControlState

  def shouldGC: Boolean
  
  def doLRA: Boolean

  def printGCDebug: Boolean
  
  
  
}