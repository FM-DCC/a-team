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
                     defs: Map[ProcName,LProc],  // definitions of processes
                     prots: Map[ProcName,GProc], // definitions of protocols
                     main: Map[Agent,LProc]):    // running (named) agents
    def ++(other:ASystem): ASystem =
      ASystem(msgs++other.msgs, defs++other.defs, prots++other.prots, main++other.main)

  object ASystem:
    val default: ASystem = ASystem(Map(),Map(),Map(),Map())

  /** Basic process (with recursive calls) */
  enum Proc[+A]:
    case End()
    case ProcCall(p:ProcName)
    case Prefix(act:A,p:Proc[A])
    case Choice(p1:Proc[A], p2:Proc[A])
    case Par(p1:Proc[A], p2:Proc[A])

  type LProc = Proc[LAct]
  type GProc = Proc[GAct]

  /** Local action, used in the semantics of individual agents. */
  enum LAct:
    case In (a:ActName, from:Set[Agent])
    case Out(a:ActName, to:Set[Agent])
    case Internal(a:ActName)
    // case IO(a:ActName, from:Set[Agent], to:Set[Agent])

  /** Global action, used in the global semantics. */
  enum GAct:
    case In (a:ActName, from:Set[Agent], to:Agent)
    case Out(a:ActName, from:Agent, to:Set[Agent])
    case IO (a:ActName, from:Set[Agent], to:Set[Agent])

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


//////////////////////////////
  // Examples and experiments //
  //////////////////////////////

//  object Examples:
//    import Program.Term._
//
//
//    val p1: Term =
//      Prefix("x",End)

