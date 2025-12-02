package ateams.backend

import Semantics.{Loc, St, getActName, nextProc, stype}
import ateams.syntax.Program.SyncType.Async
import ateams.syntax.Program.{Act, Buffer, MsgInfo, SyncType}
import ateams.syntax.Show

object BehaviourCheck:

  /** Traverse the state space, while collecting all states and edges, and collect errors/warning regarding:
   *    - deadlock
   *    - sync receptiveness
   *    - sync responsiveness
   *    - async responsiveness
   *    - too large
   *
   * @param rx input initial state
   * @param max maximum edges traversed
   * @return traversed states (processes and buffers), number of edges, list of warnings
   */
  def randomWalk(st:St, max:Int=2000): (Set[St],Int,List[String]) =
    def aux(next:Set[St], done:Set[St],
            nEdges:Int, probs:List[String],
            limit:Int): (Set[St],Int,List[String]) =
      if limit <=0 then
        // error 1: too big
        return (done,nEdges, s"Reached limit - traversed +$max edges."::probs)
      next.headOption match
        case None =>
          (done, nEdges, probs) // success
        case Some(st) if done contains st =>
          aux(next-st,done,nEdges,probs,limit)
        case Some(st) => //visiting new state
          val next2 = Semantics.next(st)
          var newProbs = checkState(st,next2) // check here for problems
          val moreSts = next2.map(_._2)
          val nNewEdges = moreSts.size
          // deadlock if there are no more steps, but there is an agent that can do an action
          if moreSts.isEmpty &&
             st.sys.main.values.exists(proc => Semantics.nextProc(proc)(using st).nonEmpty)
          then newProbs =  s"Deadlock found: ${Show.oneLine(st)}" :: newProbs
          if newProbs.exists(pr => pr.startsWith("[unbounded-loop]")) then
            (done+st,nEdges+nNewEdges,"Stopped searching after an unbounded loop."::newProbs:::probs)
          else
            aux((next-st)++moreSts,
              done+st,
              nEdges+nNewEdges,
              newProbs ::: probs,
              limit-nNewEdges)

    aux(Set(st), Set(), 0, Nil, max)

  /** Check if the current `st` has receptivenss problems (sync) knowing actions `acts` are available. */
  def checkBadSyncRecp(st:St, acts: Set[String]) : Iterable[String] =
    val canSend =
      for (agName, proc) <- st.sys.main
          case (Act.Out(aName, _), _) <- nextProc(proc)(using st)
          mi <- st.sys.msgs.get(aName).toSet
          case SyncType.Sync <- mi.st.toSet
      yield (agName, aName)
    for (agName, aName) <- canSend if !acts.contains(aName) yield
      s"[strong-receptiveness] $agName can send $aName, but the system cannot (yet) receive it: ${Show.oneLine(st)}."

  /** Check if the current `st` has responsiveness problems (sync) knowing actions `acts` are available. */
  def checkBadSyncResp(st:St, acts: Set[String]) : Iterable[String] =
    val canRecv =
      for (agName,proc) <- st.sys.main
          case (Act.In(aName,_),_) <- nextProc(proc)(using st)
          mi <- st.sys.msgs.get(aName).toSet
          case SyncType.Sync <- mi.st.toSet        yield (agName,aName)
    for (agName,aName) <- canRecv if !acts.contains(aName) yield
      s"[strong-responsiveness] $agName can receive $aName, but the system cannot (yet) send it: ${Show.oneLine(st)}."

  /** Check if the current `st` has responsiveness problems (async) knowing actions `acts` are available. */
  def checkBadAsyncResp(st: St, acts: Set[String]): Iterable[String] =
    val canARecv =
      for (agName,proc) <- st.sys.main
          case (Act.In(aName,_),_) <- nextProc(proc)(using st)
          mi <- st.sys.msgs.get(aName).toSet
          case SyncType.Async(_,_) <- mi.st.toSet        yield (agName,aName)
    for (agName,aName) <- canARecv if !acts.contains(aName) yield
      s"[strong-responsiveness] $agName can receive $aName, but the system cannot (yet) send it: ${Show.oneLine(st)}."

  /** Check if the current `st` has a loop with an async send to the same state (ignoring buffers). */
  def checkOnceUnboundedSends(st: St, next: Set[(Act,St)]): Iterable[String] =
    for case (a@Act.Out(_,_),st2) <- next
        if st.sys.main == st2.sys.main &&
          stype(a)(using st).isInstanceOf[Async] yield
      s"[unbounded-loop] can send ${Show(a)} forever: ${Show.oneLine(st)}"

  def checkOrphansTerminated(st:St): Iterable[String] =
    for (ag,p) <- st.sys.main
       if Semantics.nextProc(p)(using st).isEmpty &&
          // all terminated agents must have no incomming messages
          hasIncomming(ag,st.buffers.keys)
    yield s"[orphan-messages] Agent $ag terminated and has incomming messages: ${Show.oneLine(st)}"

  def hasIncomming(ag: String, locs: Iterable[Loc]): Boolean =
    locs.exists {
      case Loc(Some(`ag`),_) => true
      case Loc(_,Some(`ag`)) => true
      case _ => false
    }

  /**
   * Check if a given state has behavioural problems based on local information only.
   * @param st current state
   * @param next next actions and destination states
   * @return List of possible error messages
   */
  def checkState(st:St, next: Set[(Act,St)]): List[String] = {
    val acts = next.map(x => Semantics.getActName(x._1))
    checkOrphansTerminated(st).toList ::: // loop of sends
    checkOnceUnboundedSends(st,next).toList ::: // loop of sends
    checkBadSyncRecp(st,acts).toList ::: // sync receptiveness
    checkBadSyncResp(st,acts).toList ::: // sync responsiveness
    checkBadAsyncResp(st,acts).toList    // async responsiveness
  }


