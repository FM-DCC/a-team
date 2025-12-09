package ateams.backend

import Semantics.{St, aritSys, Ctx}
import ateams.syntax.{Program, Show}
import ateams.syntax.Program.{ASystem, Act, ActName, Agent, LocInfo, MsgInfo, Proc, ProcName, SyncType}

object TypeCheck:
  type Errors = Set[String]

  def pp(sy:ASystem) =
    val err = check(sy)
    if err.isEmpty then "Well-formed"
    else "Not well-formed:\n"+err.map(x => s" - $x").mkString("\n")
  def check(sy:ASystem): Errors =
    sy.defs.toSet.map(x => check(x._2)(using sy, x._1)).flatten ++
      checkBTypes(sy)
  def check(p:Proc)(using sy:ASystem, pname:ProcName): Errors =
    p match {
      case Proc.End => Set()
      case Proc.ProcCall(p) => if sy.defs.contains(p) then Set() else
        Set(s"[$pname] Unknown process call to $p.")
      case Proc.Choice(p1, p2) => check(p1)++check(p2)
      case Proc.Par(p1, p2) => check(p1)++check(p2)
      case Proc.Prefix(Act.In(act,from), p) => check(p) ++ checkAct(act,from) ++
        (sy.msgs.get(act) match
          case None => Set()//Set(s"[$pname] action $act not found.") // already checked by checkAct
          case Some(mi:MsgInfo) => mi match
            case MsgInfo(Some((i1,i2)), Some(SyncType.Sync)) =>
              if from.nonEmpty && !Semantics.inInterval(from.size,i1) then
                Set(s"[$pname] Trying to receive '$act' from {${from.mkString(",")}} but expected #{${from.mkString(",")}} ∈ {${Show.showIntrv(i1)}}.")
              else Set()
            case MsgInfo(Some((i1,i2)), Some(SyncType.Async(LocInfo(snd, rcv),_))) =>
              if from.nonEmpty && (!snd) then
                Set(s"[$pname] Receiving $act from ${from.mkString}, but no sources should be specified.")
              else if from.isEmpty && snd && i1!=(0,Some(0)) then
                Set(s"[$pname] Receiving $act with no sources, but expected some targets (in {${Show.showIntrv(i1)}}).")
              else if from.nonEmpty && snd && !Semantics.inInterval(from.size,i1) then
                Set(s"[$pname] Trying to receive '$act' from {${from.mkString(",")}} but expected #{${from.mkString(",")}} ∈ {${Show.showIntrv(i1)}}.")
              else Set()
            case _ => Set(s"Unexpected message info: ${Show(mi)}")
          )
      case Proc.Prefix(a@Act.Out(act,to), p) => check(p) ++ checkAct(act,to) ++
        (sy.msgs.get(act) match
          case None => Set() // Set(s"[$pname] action $act not found.") // already checked by checkAct
          case Some(mi:MsgInfo) => mi match
            case MsgInfo(Some((i1,i2)), Some(SyncType.Sync)) =>
              if to.nonEmpty && !Semantics.inInterval(to.size,i2) then
                Set(s"[$pname] Trying to send '$act' to {${to.mkString(",")}} but expected #{${to.mkString(",")}} ∈ {${Show.showIntrv(i2)}}.")
              else Set()
            case MsgInfo(Some((i1,i2)), Some(SyncType.Async(LocInfo(snd, rcv),_))) =>
              if to.nonEmpty && (!rcv) then
                Set(s"[$pname] Sending $act to ${to.mkString}, but no destination should be specified.")
              else if to.isEmpty && rcv && i2!=(0,Some(0)) then
                Set(s"[$pname] Sending $act with no destination, but expected some targets (in ${Show.showIntrv(i2)}).")
              else if to.nonEmpty && rcv && !Semantics.inInterval(to.size,i2) then
                Set(s"[$pname] Trying to send '$act' to {${to.mkString(",")}} but expected #{${to.mkString(",")}} ∈ {${Show.showIntrv(i2)}}.")
              else if to.isEmpty && snd && !i2._2.contains(i2._1) then
                Set(s"[$pname] Sending $act with no destination, but the *precise* number of receivers is not known (it is ${Show.showIntrv(i2)}).")
              else Set()
            case _ => Set(s"Unexpected message info: ${Show(mi)}")
          )
      case Proc.Prefix(Act.IO(a,from,to),p) => check(p) // ++ checkAct(a,from++to) // IO actions do not need to be declared
    }

  def checkAct(a:ActName, anames:Set[Agent])
              (using sy:ASystem, pname:ProcName): Errors =
    (if !sy.msgs.contains(a) then
      Set(s"[$pname] Unknown action $a.") else Set()) ++
    (for an <- (anames) if !sy.main.contains(an) yield
      s"[$pname] Unknown agent $an used by action $a.")



  /////////////////////////////////////
  // check buffer-type compatibility //
  /////////////////////////////////////
  import Semantics.{getActName,getCtx,Loc}

  /** Searches for locations that can be used by actions that require
   * different buffer types (e.g., FIFO or Unsorted), which result in
   * ill-formed programs.
   * @param st State of the system with the context
   * @return (Possibly empty) set of error messages from incompatible buffer types
   */
  def checkBTypes(sy:ASystem): Set[String] =
    val locs = getAllLocs(sy)
    val bts = for (loc,acts) <- locs yield
      (loc,acts.flatMap(act => Semantics.stype(act)(using Ctx(sy.msgs,sy.defs)) match {
        case SyncType.Sync => Set()
        case SyncType.Async(where, buf) => Set(buf.getClass.getName)
        case SyncType.Internal => Set()
      }))
    for (loc,buffs)<-bts.toSet if buffs.size>1 yield
      s"[Buffer-type] incompatible buffer types @ ${Show(loc)} (${
        buffs.map(_.toString.split('$')(1)).mkString(",")}), by messages ${
        locs(loc).map(Show.apply).mkString(", ")}."

  def getAllLocs(sy: ASystem): Map[Loc, Set[Act]] =
    val res = for (ag, p) <- sy.main yield getLocs(p, Set())(using ag, Ctx(sy.msgs,sy.defs))
    res.foldLeft(Map[Loc, Set[Act]]())(mjoin)

  /**
   * Compiles the locations and corresponding sets of actions used by a given
   * process `p` from agent `self`.
   * @param p process definition
   * @param done process names already expanded
   * @param self current agent name
   * @param ctx Context with message types and process definitions
   * @return mapping from used locations to actions that use them
   */
  def getLocs(p:Proc, done:Set[ProcName])
             (using self:Agent, ctx:Ctx): Map[Loc,Set[Act]] =
    p match
      case Proc.End => Map()
      case Proc.ProcCall(p) if done(p) => Map()
      case Proc.ProcCall(p) => getLocs(ctx.defs(p),done+p)
      case Proc.Choice(p1, p2) =>
        mjoin( getLocs(p1,done), getLocs(p2,done)) // could enrich one of the "done"s
      case Proc.Par(p1, p2) =>
        mjoin( getLocs(p1,done), getLocs(p2,done)) // could enrich one of the "done"s
      case Proc.Prefix(act, p) =>
        val rest = getLocs(p,done)
        Semantics.stype(act) match
          case SyncType.Sync => rest
          case SyncType.Internal => rest
          case SyncType.Async(where, buf) =>
            val sndrcvs = getSndRcv(self,act,where)
            val locs = for (Loc(snd,rcv),a)<-sndrcvs yield
                       Semantics.getLoc(where,a,snd,rcv) -> Set(a)
            mjoin(rest,locs.toMap)

  private def getSndRcv(self: Agent, act: Act, li:LocInfo)
      : Set[(Loc,Act)] =
    (act,li) match
      case (Act.In(_, from),LocInfo(true, hasRcv)) =>
        for f<-from yield Loc(Some(f), Option.when(hasRcv)(self)) -> act
      case (Act.In(_, _),LocInfo(false, hasRcv)) =>
        Set(Loc(None, Option.when(hasRcv)(self)) -> act)
      case (Act.Out(_, to),LocInfo(hasSnd, true)) =>
        for t<-to yield Loc(Option.when(hasSnd)(self), Some(t)) -> act
      case (Act.Out(_, _),LocInfo(hasSnd, false)) =>
        Set(Loc(Option.when(hasSnd)(self), None) -> act)
      case (Act.IO(a, from, to),_) => Set()

  /** Join of two relations AxB defined as a mapping A->Set[B]. */
  def mjoin[A,B](m1:Map[A,Set[B]], m2:Map[A,Set[B]]) =
    m1 ++ (for (k,v)<-m2 yield k -> (m1.getOrElse(k,Set()) ++ v))
