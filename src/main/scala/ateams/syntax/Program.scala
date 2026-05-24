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
                     defs: Map[ProcName,LProc], // definitions of processes
                     main: Map[Agent,LProc]): // running (named) agents
    def ++(other:ASystem): ASystem =
      ASystem(msgs++other.msgs, defs++other.defs, main++other.main)

  object ASystem:
    val default: ASystem = ASystem(Map(),Map(),Map())

  /** Basic process (with recursive calls) */
  enum Proc[A]:
    case End()
    case ProcCall(p:ProcName)
    case Prefix(act:A,p:Proc[A])
    case Choice(p1:Proc[A], p2:Proc[A])
    case Par(p1:Proc[A], p2:Proc[A])

  type LProc = Proc[LAct]
  type GProc = Proc[GAct]

  /** Action (in, out, or tau) */
  enum LAct:
    case In(a:ActName, from:Set[Agent])
    case Out(a:ActName, to:Set[Agent])
    case Internal(a:ActName)
    // case IO(a:ActName, from:Set[Agent], to:Set[Agent])

  /** Global action, used in the global semantics. */
  enum GAct:
    case In( a:ActName, from:Set[Agent], to:Agent)
    case Out(a:ActName, from:Agent, to:Set[Agent])
    case IO( a:ActName, from:Set[Agent], to:Set[Agent])

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

  //// Protocol
  // enum Protocol:
  //   case Action(act:Act)
  //   case Seq(p1:Protocol, p2:Protocol)
  //   case Choice(p1:Protocol, p2:Protocol)
  //   case Par(p1:Protocol, p2:Protocol)
  //   case Rec(p:Protocol)

  //   def toASystem: ASystem =
  //     def rec(p:Protocol): (ASystem, Proc) = p match
  //       case Action(act) => (ASystem.default, Proc.Prefix(act, Proc.End))
  //       case Seq(p1, p2) =>
  //         val (sys1, proc1) = rec(p1)
  //         val (sys2, proc2) = rec(p2)
  //         ??? //(sys1 ++ sys2, Proc.Seq(proc1, proc2))
  //       case Choice(p1, p2) =>
  //         val (sys1, proc1) = rec(p1)
  //         val (sys2, proc2) = rec(p2)
  //         (sys1 ++ sys2, Proc.Choice(proc1, proc2))
  //       case Par(p1, p2) =>
  //         val (sys1, proc1) = rec(p1)
  //         val (sys2, proc2) = rec(p2)
  //         (sys1 ++ sys2, Proc.Par(proc1, proc2))
  //       case Rec(p) =>
  //         val (sys, proc) = rec(p)
  //         // Here we should ideally generate a fresh name for the recursive call
  //         // to avoid clashes with other definitions. For simplicity, we use a fixed name.
  //         val recName = "rec"
  //         (ASystem.default.copy(defs = Map(recName -> proc)) ++ sys, Proc.ProcCall(recName))

  //     rec(this)._1
  // //// Preprocess

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

