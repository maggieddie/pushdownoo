package org.ucombinator.dsg

/************************
 * Stack Action markers
 ************************/
abstract class StackAction [+F]

/**
 * Stack Unchanged
*/

case object Eps extends StackAction[Nothing]

/**
 * Push a frame
 */
case class Push[F](frame: F) extends StackAction[F]

/** 
 *  Pop a frame
 **/
case class Pop[F](frame: F) extends StackAction[F]

/**
 *  Pop one frame, push another one // Not sure where to use for this case
 */
case class Switch[F,S] (popped: F, target: S, pushed: F) extends StackAction[F]

object StackActionKind extends Enumeration {
  type StatckActionKind = Value
  val Eps, Pop, Push = Value
}