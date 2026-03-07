package ateams.syntax

import Buffer.*


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

