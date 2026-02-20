package ateams.syntax

import scala.collection.immutable.Queue
import caos.common.Multiset as MSet
/**
 * Internal structure to represent terms in A-Teams.
 */

object Program:

  type Agent = String // helper
  type ActName = String // helper
  type ProcName = String // helper

  case class ASystem(msgs: Map[ActName,MsgInfo], // message declarations
                     defs: Map[ProcName,Proc], // definitions of processes
                     main: Map[Agent,Proc]): // running (named) agents
    def ++(other:ASystem): ASystem =
      ASystem(msgs++other.msgs, defs++other.defs, main++other.main)

  object ASystem:
    val default: ASystem = ASystem(Map(),Map(),Map())

  /** Basic process (with recursive calls) */
  enum Proc:
    case End
    case ProcCall(p:ProcName)
    case Prefix(act:Act,p:Proc)
    case Choice(p1:Proc, p2:Proc)
    case Par(p1:Proc, p2:Proc)

  /** Action (in, out, or tau) */
  enum Act:
    case In(a:ActName, from:Set[Agent])
    case Out(a:ActName, to:Set[Agent])
    case IO(a:ActName, from:Set[Agent],to:Set[Agent])

  /** Fields for the declaration of a message */
  case class MsgInfo(arity: Option[(Intrv,Intrv)], st:Option[SyncType])
  object MsgInfo:
    val defaultArity: (Intrv,Intrv) = (1,Some(1))->(1,Some(1))
    val defaultST: SyncType = SyncType.Sync

  type Intrv = (Int,Option[Int])
  enum SyncType:
    case Sync
    case Async(where:LocInfo, buf: Buffer)
    case Internal
    // case Fifo(where:LocInfo)
    // case Unsorted(where: LocInfo)

  case class LocInfo(snd:Boolean, rcv:Boolean)

  sealed trait Buffer:
    def +(el:ActName): Buffer
    def -(el:ActName): Option[Buffer]
    def isEmpty: Boolean
    def contains(el:ActName): Boolean
    def size: Int

  case class Fifo(q:Queue[ActName]) extends Buffer:
    def +(el:ActName) = Fifo(q.enqueue(el))
    def -(el:ActName): Option[Buffer] = q.dequeueOption match
      case Some((a,q2)) if a==el => Some(Fifo(q2))
      case _ => None
    def isEmpty = q.isEmpty
    def contains(el:ActName) = q.contains(el)
    def size = q.size
  object Fifo:
    def apply():Fifo = Fifo(Queue[ActName]())

  case class Unsorted(m:MSet[ActName]) extends Buffer:
    def +(el:ActName) = Unsorted(m+el)
    def -(el:ActName): Option[Buffer] =
      if m.contains(el) then Some(Unsorted(m-el)) else None
    def isEmpty = m.isEmpty
    def contains(el: ActName) = m.contains(el)
    def size = m.data.values.sum
  object Unsorted:
    def apply():Unsorted = Unsorted(MSet[ActName]())



  //// Preprocess

  def preProcess(sys: ASystem): ASystem =
    sys.msgs.get("default") match
      case Some(mi) =>
        sys.copy(msgs =
          sys.msgs.map((k,v) => (k, v.copy(
            arity = v.arity.orElse(mi.arity).orElse(Some(MsgInfo.defaultArity)),
            st = v.st.orElse(mi.st).orElse(Some(MsgInfo.defaultST)),
          ))) // + ("tau" -> MsgInfo(None,None))
        )
      case None => sys


//////////////////////////////
  // Examples and experiments //
  //////////////////////////////

//  object Examples:
//    import Program.Term._
//
//
//    val p1: Term =
//      Prefix("x",End)

