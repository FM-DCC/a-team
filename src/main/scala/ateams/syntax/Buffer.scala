package ateams.syntax

import ateams.syntax.Program.*
import Buffer.*
import scala.collection.immutable.Queue
import caos.common.Multiset as MSet
import scala.collection.mutable.PriorityQueue

/**
 * Internal structure to represent buffers in A-Teams.
 */
sealed trait Buffer:
  def +(el:ActName): Buffer
  def -(el:ActName): Option[Buffer]
  def isEmpty: Boolean
  def size: Int

object Buffer:
  /** A FIFO buffer, where messages are received in the order they
   *  were sent. The '+' operation enqueues a message, and the '-'
   *  operation dequeues a message if it matches the expected one
   *  (the head of the queue). 
   * */
  case class Fifo(q:Queue[ActName]) extends Buffer:
    def +(el:ActName) = Fifo(q.enqueue(el))
    def -(el:ActName): Option[Buffer] = q.dequeueOption match
      case Some((a,q2)) if a==el => Some(Fifo(q2))
      case _ => None
    def isEmpty = q.isEmpty
    def size = q.size
  object Fifo:
    def apply():Fifo = Fifo(Queue[ActName]())

  /** An unsorted buffer, where messages are received in any order.
   * The '+' operation adds a message to the multiset, and the '-'
   * operation removes a message if it is present in the multiset.
   * */
  case class Unsorted(m:MSet[ActName]) extends Buffer:
    def +(el:ActName) = Unsorted(m+el)
    def -(el:ActName): Option[Buffer] =
      if m.contains(el) then Some(Unsorted(m-el)) else None
    def isEmpty = m.isEmpty
    def size = m.data.values.sum
  object Unsorted:
    def apply():Unsorted = Unsorted(MSet[ActName]())

  /** A priority queue buffer, where messages are received based
   * on a priority order (e.g., lexicographic order of message
   * names). The '+' operation adds a message to the priority
   * queue, and the '-' operation removes a message if it is the 
   * one with the highest priority (the head of the queue).
   * */
  case class PrioQueue(q:PriorityQueue[ActName]) extends Buffer:
    def +(el:ActName) =
      val q2 = q.clone()
      q2.enqueue(el)
      PrioQueue(q2) 
    def -(el:ActName): Option[Buffer] =
      q.headOption match
        case Some(a) if a==el =>
          val q2 = q.clone()
          q2.dequeue
          Some(PrioQueue(q2))
        case _ => None
    def isEmpty = q.isEmpty
    def size = q.size
  object PrioQueue:
    def apply():PrioQueue =
      PrioQueue(PriorityQueue[ActName]()(
        Ordering.by((a:ActName) => a).reverse))
  

