package ateams.backend

import caos.sos.SOS
import caos.common.Multiset as MSet
import ateams.backend.Semantics.St
import ateams.syntax.{Program, Show}
import Program.*
import Proc.*
import ateams.syntax.Program.Act.Out
import ateams.syntax.Program.SyncType.{Async, Internal, Sync}

//import scala.collection.{immutable, mutable}
import scala.collection.immutable.Queue
import scala.language.implicitConversions

/** Small-step semantics for both commands and boolean+integer expressions.  */
object Semantics extends SOS[Act,St]:

  /**
   * State of our operational semantics.
   * @param sys has the full program, including the current state of the local processes.
   * @param buffers contain all the buffers storing messages in transit.
   */
  case class St(sys: ASystem,
                buffers: Map[Loc,Buffer])
  case class Ctx(msgs: Map[ActName,MsgInfo], // message declarations
                 defs: Map[ProcName,Proc]) // definitions of processes)

  case class Loc(snd: Option[String], rcv: Option[String])
  type Defs = Map[String,Proc]
  type Procs = Map[String,Proc]

  /** What are the set of possible evolutions (label and new state) */
  def next[A>:Act](st: St): Set[(A, St)] =
    nextState(st).asInstanceOf[Set[(A,St)]]

  /**
   * Possible evolution from a given state
   * @param st current state to evaluate
   * @return set of pairs (a',st') with the possible actions and next states
   */
  def nextState(st:St): Set[(Act,St)] =
    implicit val state:St = st
    val canGo =
      for (n,p) <- st.sys.main.toSet; (a,p2) <- nextProc(p)
      yield a -> (n->p2)
    updateSt(nextIO(canGo)) ++
    updateSt(nextSync(canGo)) ++
    nextSend(canGo) ++
    nextRcv(canGo)

  private def updateSt(aps: Set[(Act,Procs)])(using st:St): Set[(Act,St)] =
    for (a,ps) <- aps yield
      a -> st.copy( sys = st.sys.copy( main = st.sys.main ++ ps))


  ///////////////////////////////////
  // Semantics of the "tau" action //
  ///////////////////////////////////

  /** How to evolve a state using internal actions. */
  def nextIO(canGo: Set[(Act,(String,Proc))])(using st:St): Set[(Act,Procs)] =
    for case (a@Act.IO(_,_,_),(n,p)) <- canGo
      yield  a -> Map(n->p) // just the updates

  ///////////////////////////////////////
  // Semantics of synchronous messages //
  ///////////////////////////////////////

  /** How to evolve a state using syncrhonous actions. */
  def nextSync(canGo: Set[(Act,(String,Proc))])(using st:St): Set[(Act,Procs)] = {
    implicit val ctx = Ctx(st.sys.msgs,st.sys.defs)
    // compile map "action-name" -> ([("snd-agent","nextProc","rcv-agt?"), ...] , [("rcv-agent","nextProc","snd-agt?"), ...])
    var syncsMap = Map[String,(List[(String,Proc,Set[String])],List[(String,Proc,Set[String])])]()
    for (a,(n,p)) <- canGo if stype(a) == SyncType.Sync
        do {
      a match
        case Act.In(s,from) if syncsMap.contains(s) =>
          syncsMap = syncsMap + (s -> (syncsMap(s)._1,(n,p,from)::syncsMap(s)._2))
        case Act.In(s,from) =>
          syncsMap = syncsMap + (s -> (Nil,List((n,p,from))))
        case Act.Out(s,to) if syncsMap.contains(s) =>
          syncsMap = syncsMap + (s -> ((n,p,to)::syncsMap(s)._1,syncsMap(s)._2 ))
        case Act.Out(s,to) =>
          syncsMap = syncsMap + (s -> (List((n,p,to)),Nil))
        case _ =>
    }

    val combs: Iterable[(String, List[(String,Proc,Set[String])], List[(String,Proc,Set[String])])] =
      (for
        (a,(snds,rcvs)) <- syncsMap
      yield
        val smin = arit(a)._1._1
        val smax = arit(a)._1._2.getOrElse(snds.size) min snds.size
        val rmin = arit(a)._2._1
        val rmax = arit(a)._2._2.getOrElse(rcvs.size) min rcvs.size
        for ssize <- smin to smax
            rsize <- rmin to rmax
            sset <- snds combinations ssize
            rset <- rcvs combinations rsize
            // only if all non-empty sets in sset are represented in rset and vice-versa!
            if sset.forall( (_,_,tos)   => tos.isEmpty   || tos==rset.map(_._1).toSet) &&
               rset.forall( (_,_,froms) => froms.isEmpty || froms==sset.map(_._1).toSet)
        yield {
          (a,sset,rset)
        }
        ).flatten

    val combss = combs.toSet
    for (a,snds,rcvs) <- combs.toSet yield
      Act.IO(a,snds.map(_._1).toSet++rcvs.flatMap(_._3).toSet,
               rcvs.map(_._1).toSet++snds.flatMap(_._3).toSet)
        -> (snds.map((a,b,_)=>(a,b)) ++ rcvs.map((a,b,_)=>(a,b))).toMap // just the updates
  }

  //////////////////////////////////////////
  // Semantics of the FIFO/unsorted sends //
  //////////////////////////////////////////

  /** How to evolve a state using asyncrhonous sending actions. */
  def nextSend(canGo: Set[(Act,(Agent,Proc))])(using st:St): Set[(Act,St)] = {
    implicit val ctx = Ctx(st.sys.msgs,st.sys.defs)
    //println(s"## can send? ${canGo.map((a,ag)=>s"\n  - ${Show(a)} @ ${ag._1}").mkString}") //\nstype(${canGo.tail.head._1}) = ${stype(canGo.tail.head._1)}")
    (for case (a@Act.Out(s,to), (n, p)) <- canGo
        if isAsync(stype(s)) //  List(SyncType.Fifo,SyncType.Unsorted) contains stype(a)
     yield {
        //checkType(a,stype(s)) // proper runtime error if not enough information in Act
       //println(s"SENDING $a...")
       // need to send one message for each destination. Options:
       //  1. we do not care to who - need to count sends (arity or to - error if not possible)
       //  2. we do care to who, but not know them (to={}) - error (not possible)
       //  3. we do care to who, and to!={}, but arity does not match "to" - error (not possible)
       //  4. we do care to who, and to!={}, and arity matches "to" - send to each "to"
       (stype(s),arit(s),to.isEmpty) match
         // case 1: do not care to who, "to" exists, arity must match
         case (Async(LocInfo(locSnd,false), buff), art, false)  =>
//           println("-- case 1")
//           if !inInterval(to.size,art._2) then
//             sys.error(s"Trying to send '${Show(a)}' to {${to.mkString}} but expected # in interval {${Show.showIntrv(art._2)}}.")
//           val loc = getLoc(s, if locSnd then Some(n) else None, None)
//           var buffer = st.buffers.getOrElse[Buffer](loc, buff)
//           for _ <- to  do buffer = buffer + s
//           updateSt(a,loc,buffer,n,p)
           // UPDATED case 1: "to" should not exist
           sys.error(s"Trying to send '${Show(a)}' to {${to.mkString}} but there is no guarantees that the receivers are the correct ones. You should either remove the explicit set of senders, or change the synchronisation type.")

         // case 2: do not care to who, "to" does not exists, arity must be precise
         case (Async(LocInfo(locSnd,false), buf), art, true)  =>
           //println("-- case 2")
           if !art._2._2.contains(art._2._1) then
             sys.error(s"Trying to send '${Show(a)}' to {${Show.showIntrv(art._2)}} without having a single number of destinations (${Show.showIntrv(art._2)}).")
           val loc = getLoc(s, if locSnd then Some(n) else None, None)
           var buffer = st.buffers.getOrElse(loc, buf)
           checkBuffType(buffer,buf,s"sending ${Show(a)}")
           for _ <- 1 to art._2._1 do buffer = buffer + s
           updateSt(a,loc,buffer,n,p)

         // case 3: care to who, but "to" does not exists - error unless it is meant to send to none (state unchanged)
         case (Async(LocInfo(locSnd,true), buf), art, true)  =>
           //println("-- case 3")
           if art._2._1 == 0 && art._2._2 == Some(0) then
             a -> st.copy( sys = st.sys.copy( main = st.sys.main + (n->p)) )
           else
            sys.error(s"Trying to send '${Show(a)}' to {${Show.showIntrv(art._2)}} - missing precise destinations")

         // case 4: care to who, and "to" exists, arity must match
         case (Async(LocInfo(locSnd,true), buf), art, false)  =>
           //println("-- case 4")
           if !inInterval(to.size,art._2) then
             sys.error(s"Trying to send '${Show(a)}' to {${to.mkString(",")}} but expected #{${to.mkString(",")}} ∈ {${Show.showIntrv(art._2)}}.")
           //var queues: List[(Loc,Queue[ActName])] = Nil
           val newBuffers = for ag <- to yield
             val loc = getLoc(s, if locSnd then Some(n) else None, Some(ag))
             val buffer = st.buffers.getOrElse(loc, buf) + s
             checkBuffType(buffer,buf,s"sending ${Show(a)}")
             loc -> buffer
           updateSt(a,newBuffers,n,p)

         // any other case (fifo or sync)
         case _ => sys.error(s"case not supported for sending ${Show(a)}")
     })
  }

  /** Checks if a number `n` is in the bounds of an interval `intrv`. */
  def inInterval(n: Int, intr: Intrv): Boolean =
    n >= intr._1 && (intr._2.isEmpty || n <= intr._2.get)

  private def getRcv(ag: String, styp: SyncType): Option[String] =
    styp match
      case Async(LocInfo(_,true),_) => Some(ag)
      case _ => None

  //////////////////////////////////////////
  // Semantics of the FIFO/unsorted sends //
  //////////////////////////////////////////

  /** How to evolve a state using asyncrhonous receiving actions. */
  def nextRcv(canGo: Set[(Act,(Agent,Proc))])(using st:St): Set[(Act,St)] =
    implicit val ctx = Ctx(st.sys.msgs,st.sys.defs)
    //println(s"## can receive?") // ${canGo.map((a,ag)=>s"\n  - ${Show(a)} @ ${ag._1}").mkString}") //\nstype(${canGo.tail.head._1}) = ${stype(canGo.tail.head._1)}")
    (for case (a@Act.In(s,from), (n, p)) <- canGo
        if isAsync(stype(s))
     yield {
       //println(s"RECEIVING $a...")
       // need to receive one message for each sender. Options:
       //  - we do not care from who - need to count receives (arity or to - error if not possible)
       //  - we do care from who, but not know them (from={}) - error (not possible)
       //  - we do care from who, and from!={}, but arity does not match "from" - error (not possible)
       //  - we do care from who, and from!={}, and arity matches "from" - receive from each available "from"
       (stype(s),arit(s),from.isEmpty) match {
         // case 1: do not care from who, "from" exists, arity must match
         case (Async(LocInfo(false,locRcv), buf), art, false)  =>
//           //println("-- case 1")
//           if !inInterval(from.size,art._1) then
//             sys.error(s"Trying to get '${Show(a)}' from {${from.mkString}} but expected #{${from.mkString}} ∈ {${Show.showIntrv(art._1)}}.")
//           val loc = getLoc(s, None, if locRcv then Some(n) else None)
//           var buffer: Option[Buffer] = Some(st.buffers.getOrElse[Buffer](loc, buf))
//           for _ <- from  do buffer = buffer.flatMap(_-s)
////           getFromBuffers(a,n,p,Map(loc->buffer))
//           updateOptSt(a,loc,buffer,n,p)
           // UPDATED case 1: "from" should not exist
           sys.error(s"Trying to get '${Show(a)}' from {${from.mkString}} but there is no guarantees that the senders are the correct ones. You should either remove the explicit set of receivers, or change the synchronisation type.")

         // case 2: do not care from who, "from" does not exists, arity must be precise
         case (Async(LocInfo(false,locSnd), buf), art, true)  =>
           //println("-- case 2")
           if !art._1._2.contains(art._1._1) then
             sys.error(s"Trying to get '${Show(a)}' from {${Show.showIntrv(art._1)}} without having a single number of sources (${Show.showIntrv(art._1)}).")
           val loc = getLoc(s, None, if locSnd then Some(n) else None)
           var buffer: Option[Buffer] = Some(st.buffers.getOrElse(loc, buf))
           for (b<-buffer) do checkBuffType(b,buf,s"getting ${Show(a)}")
           for _ <- 1 to art._1._1 do buffer = buffer.flatMap(_ - s)
           updateOptSt(a,loc,buffer,n,p)

         // case 3: care to who, but "to" does not exists - error unless it is meant to send to none (state unchanged)
         case (Async(LocInfo(true,locSnd), buf), art, true)  =>
           //println("-- case 3")
           if art._1._1 == 0 && art._1._2 == Some(0) then
             Set(a -> st.copy( sys = st.sys.copy( main = st.sys.main + (n->p)) ))
           else
            sys.error(s"Trying to get '${Show(a)}' from {${Show.showIntrv(art._1)}} - missing precise sources")

         // case 4: care to who, and "to" exists, arity must match
         case (Async(LocInfo(true,locSnd), buf), art, false)  =>
           //println("-- case 4")
           if !inInterval(from.size,art._1) then
             sys.error(s"Trying to get '${Show(a)}' from {${from.mkString}} but expected #{${from.mkString}} ∈ {${Show.showIntrv(art._1)}}.")
           val newBuffers = for ag <- from yield
             val loc = getLoc(s, Some(ag), if locSnd then Some(n) else None)
             val buffer = Some(st.buffers.getOrElse(loc, buf)).flatMap(_ - s)
             for (b<-buffer) do checkBuffType(b,buf,s"getting ${Show(a)}")
             buffer.map(loc -> _)
           //println(s"!!! new buffers: ${newBuffers}")
           updateOptSt(a,newBuffers,n,p)

         case _ => sys.error(s"case not supported for receiving: ${Show(a)}")
       }
     }).flatten


  private def updateSt(a:Act, loc:Loc, buffer: Buffer
                       ,n:ActName, p:Proc)(using st:St): (Act,St) =
    a -> st.copy(sys = st.sys.copy(main = st.sys.main + (n -> p)),
                 buffers = if buffer.isEmpty then st.buffers - loc
                           else st.buffers + (loc -> buffer))
  private def updateOptSt(a:Act, loc:Loc, buffer: Option[Buffer]
                      ,n:ActName, p:Proc)(using st:St): Option[(Act,St)] =
    buffer.map(updateSt(a,loc,_,n,p))

  private def updateSt(a:Act, buffers: Set[(Loc,Buffer)]
                      ,n:ActName, p:Proc)(using st:St): (Act,St) =
    a -> st.copy( sys = st.sys.copy( main = st.sys.main + (n->p)),
                  buffers = (st.buffers ++ buffers) -- buffers.filter(_._2.isEmpty).map(_._1))
  private def updateOptSt(a:Act, buffers: Set[Option[(Loc,Buffer)]]
                      ,n:ActName, p:Proc)(using st:St): Set[(Act,St)] =
    if buffers.contains(None) then Set()
    else Set(updateSt(a,buffers.flatten,n,p))
      //Set(a -> st.copy( sys = st.sys.copy( main = st.sys.main + (n->p)),
        //          buffers = st.buffers ++ buffers.flatten.toMap))

  ////////////////////////////
  // Semantics of a process //
  ////////////////////////////

  /** How to evolve an individual process, as in CCS. */
  def nextProc(p:Proc)(using st:St): Set[(Act,Proc)] = p match
    case End => Set()
    case ProcCall(p) => nextProc(st.sys.defs.getOrElse(p,End))
    case Prefix(act,p) => Set(act -> p)
    case Choice(p1,p2) =>
      nextProc(p1) ++ nextProc(p2)
    case Par(p1, p2) =>
      val nx1 = nextProc(p1)
      val nx2 = nextProc(p2)
      (for (n,s)<-nx1 yield n->Par(s,p2)) ++
      (for (n,s)<-nx2 yield n->Par(p1,s))


  //////////////////////////
  // Auxiliarly functions //
  //////////////////////////

  implicit def getActName(a:Act): String = a match
    case Act.In(s,_) => s
    case Act.Out(s,_) => s
    case Act.IO(s,_,_) => s
  implicit def getCtx(st:St): Ctx =
    Ctx(st.sys.msgs,st.sys.defs)

  def getLoc(act: String, snd:Option[String], rcv: Option[String])(using ctx:Ctx): Loc =
    stype(act) match
      case Sync => sys.error(s"Cannot get buffers of sync message \"$act\"")
      case Async(locInfo,_)     => getLoc(locInfo,act,snd,rcv)
      case Internal => sys.error(s"Cannot get buffers of internal messages")

  def getLoc(linfo: LocInfo, act:String,
                     snd: Option[String],
                     rcv: Option[String]): Loc = (linfo,snd,rcv) match
    case (LocInfo(true,true),Some(s),Some(r)) => Loc(snd,rcv)
    case (LocInfo(true,false),Some(s),_) => Loc(snd,None)
    case (LocInfo(false,true),_,Some(r)) => Loc(None,rcv)
    case (LocInfo(false,false),_,_) => Loc(None,None)
    case _ => sys.error(s"Missing information for channel \"$act\": type ${Show(linfo)}, got $snd->$rcv.")


  def arit(act: String)(using ctx:Ctx): (Intrv, Intrv) =
    aritSys(act)
  def aritSys(act: String)(using ctx:Ctx): (Intrv, Intrv) =
    ctx.msgs.get(act).flatMap(_.arity).getOrElse(sys.error(s"[ar] Unknown action $act."))

  def stype(act: String)(using ctx:Ctx): SyncType =
    ctx.msgs.get(act).flatMap(_.st).getOrElse(Internal)//sys.error(s"[st] Unknown action $act."))

  def isAsync(syncType: Program.SyncType): Boolean =
    syncType match
      case SyncType.Async(_, _) => true
      case _ => false

  private def checkBuffType(b1: Buffer, b2: Buffer, msg: String): Unit =
    if b1.getClass != b2.getClass then
      sys.error(s"Conflicting buffer types when $msg: had type ${
        b1.getClass.toString.split('$')(1)} and now has type ${
        b2.getClass.toString.split('$')(1)}.")
