package ateams.backend

import Semantics.{St, aritSys}
import ateams.syntax.{Program, Show}
import ateams.syntax.Program.{ASystem, Act, ActName, Agent, LocInfo, MsgInfo, Proc, ProcName, SyncType}

object TypeCheck:
  type Errors = Set[String]

  def pp(st:St) =
    val err = check(st)
    if err.isEmpty then "Well-formed"
    else "Not well-formed:\n"+err.map(x => s" - $x").mkString("\n")
  def check(st:St): Errors =
    check(st.sys) ++ checkBTypes(st)
  private def check(sy:ASystem): Errors =
    sy.defs.toSet.map(x => check(x._2)(using sy, x._1)).flatten
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
  import Semantics.{getActName,Loc}

  def getAllLocs(st:St): Map[Loc,Set[Act]] =
    val res = for (ag,p)<- st.sys.main yield getLocs(p,Set())(using ag,st)
    res.foldLeft(Map[Loc,Set[Act]]())(mjoin)

  def checkBTypes(st:St): Set[String] = // Map[Loc, Set[Class[? <: Program.Buffer]]] =
    val locs = getAllLocs(st)
    val bts = for (loc,acts) <- locs yield
      (loc,acts.flatMap(act => Semantics.stype(act)(using st) match {
        case SyncType.Sync => Set()
        case SyncType.Async(where, buf) => Set(buf.getClass.getName)
        case SyncType.Internal => Set()
      }))
    for (loc,buffs)<-bts.toSet if buffs.size>1 yield
      s"[Buffer-type] incompatible buffer types @ ${Show(loc)} (${
        buffs.map(_.toString.split('$')(1)).mkString(",")}), by messages ${
        locs(loc).map(Show.apply).mkString(", ")}."

  def getLocs(p:Proc, done:Set[ProcName])
             (using self:Agent, st:St): Map[Semantics.Loc,Set[Act]] =
    p match
      case Proc.End => Map()
      case Proc.ProcCall(p) if done(p) => Map()
      case Proc.ProcCall(p) => getLocs(st.sys.defs(p),done+p)
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


  def mjoin[A,B](m1:Map[A,Set[B]], m2:Map[A,Set[B]]) =
    m1 ++ (for (k,v)<-m2 yield k -> (m1.getOrElse(k,Set()) ++ v))
