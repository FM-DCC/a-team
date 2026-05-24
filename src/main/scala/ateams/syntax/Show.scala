package ateams.syntax

import ateams.syntax.Program.*
import ateams.backend.Semantics.*
import Buffer.*

/**
 * List of functions to produce textual representations of commands
 */
object Show:

  //def justTerm(s: CCSSystem): String = apply(s.main)

  def apply(st: St): String =
    short(st.sys) + (if st.buffers.isEmpty then "" else "\n") +
      showBuffers(st)
//      (if st.fifos.isEmpty then "" else "\n") +
//      showFifos(st) + (if st.msets.isEmpty then "" else "\n") +
//      showMSets(st) +
//      "\n-- "+st.buffers.mkString("/")

  def apply(s: ASystem): String = {
    (if s.msgs.nonEmpty then s"msgs:\n${showMsgs(s.msgs)}\n" else "") +
    (if s.defs.nonEmpty then s"defs:\n${showDefs(s.defs)}\n" else "") +
    (if s.prots.nonEmpty then s"protocols:\n${showProts(s.prots)}\n" else "") +
    s"main:\n  ${showMain(s.main)}"
  }

  def short(s:ASystem): String =
    (for (nm, p) <- s.main yield s"$nm: ${apply(p)}")
      .mkString("\n")

  def oneLine(st: St): String =
    ((for (nm, p) <- st.sys.main yield s"$nm: ${apply(p)}") ++
    (for (loc, buf) <- st.buffers yield s"${apply(loc)}=>${showBuf(buf)}"))
        .mkString("{",", ","}")

  def showMsgs(msgs: Map[String,MsgInfo]): String =
    (for (m,info)<-msgs if m!="default" yield s"  $m: ${apply(info)}")
      .mkString("\n")

  def apply(msgInfo: MsgInfo): String = {
    val miArity = msgInfo.arity.getOrElse(MsgInfo.defaultArity)
    val miST = msgInfo.st.getOrElse(MsgInfo.defaultST)
    s"${showIntrv(miArity._1)} → ${showIntrv(miArity._2)}, " +
    miST.match {
      case Program.SyncType.Sync => "sync"
      case Program.SyncType.Async(where,buf:Fifo) => s"Fifo @ ${apply(where)}"
      case Program.SyncType.Async(where,_:Unsorted) => s"Unsorted @ ${apply(where)}"
      case Program.SyncType.Async(where,_:PrioQueue) => s"PrioQueue @ ${apply(where)}"
      case Program.SyncType.Internal => "internal"
    }
  }

  def showDefs(ds: Map[String,LProc]): String =
    (for (nm,p) <- ds yield s"  $nm := ${apply(p)}")
      .mkString("\n")
  def showProts(ds: Map[String,GProc]): String =
    (for (nm,p) <- ds yield s"  $nm := ${apply(p)}")
      .mkString("\n")
  def showMain(m: Map[String,LProc]): String =
    (for (nm,p) <- m yield s"$nm: ${apply(p)}")
      .mkString(" || ")
  def showIntrv(intr: Intrv): String = intr._2 match
    case Some(n) if intr._1==n => n.toString
    case Some(n) => s"${intr._1}..$n"
    case None => s"${intr._1}..∞"

  def apply[A](p: Proc[A]): String = p match
    case Proc.End() => "skip"
    case Proc.ProcCall(p) => p
    case Proc.Prefix(act, Proc.End()) => apply[A](act)
    case Proc.Prefix(act, t) => s"${apply[A](act)}.${applyP[A](t)}"
    case Proc.Choice(t1, t2) => s"${applyP[A](t1)}+${applyP[A](t2)}"
    case Proc.Par(t1, t2) => s"${applyP[A](t1)} | ${applyP[A](t2)}"

  private def applyP[A](p: Proc[A]): String = p match
    case _:(Proc.End[A] | Proc.ProcCall[A] | Proc.Prefix[A]) => apply[A](p)
    case _ => s"(${apply[A](p)})"

  def apply[A](a:A): String = a match
    case LAct.In(s,from) => s"$s?${from.mkString(",")}"
    case LAct.Out(s,to) => s"$s!${to.mkString(",")}"
    case LAct.Internal("tau") => s"τ"
    case LAct.Internal(s) => s"[$s]"
  // def apply(a:GAct): String = a match
    case GAct.In(s, from, to) if from.isEmpty => s"$s?$to"
    case GAct.Out(s, from, to) if to.isEmpty => s"$s?$from"
    case GAct.In(s, from, to) => s"$s?${from.mkString(",")}-$to"
    case GAct.Out(s, from, to) => s"$s!$from-${to.mkString(",")}"
    case GAct.IO("tau",_,_) => s"τ"
    case GAct.IO(s,from,to) if from.isEmpty && to.isEmpty => s"[$s]"
    case GAct.IO(s,from,to) => s"${agSet(from)}→${agSet(to)}:$s"
  private def agSet(s:Set[_]): String =
    if s.isEmpty then "∅" else s.mkString(",")

  def apply(l:LocInfo): String = (l.snd,l.rcv) match
    case (false,false) => "globally"
    case (true,false) => "sender"
    case (false, true) => "receiver"
    case (true,true) => "sender&receiver"

  //////////
  // Runtime semantics
  ///////////
  def showBuffers(st: St): String =
    (for (loc,buf) <- st.buffers yield s"${apply(loc)} => ${showBuf(buf)}")
      .mkString("\n")
  def showBuf(b:Buffer): String = b match
    case Fifo(q) => s"[${q.mkString(",")}]"
    case Unsorted(m) => s"{$m}"
    case PrioQueue(q) => s"[${q.mkString(",")}]"
                        //s"[${q.toList.sorted.mkString(",")}]"


//  def showFifos(st: St): String =
//    (for (loc,queue) <- st.fifos yield s"${apply(loc)} => [${queue.mkString(",")}]")
//      .mkString("\n")
//
//  def showMSets(st: St): String =
//    (for (loc,mset) <- st.msets yield s"${apply(loc)} => {${mset}}")
//      .mkString("\n")

  def apply(l:Loc): String = (l.snd,l.rcv) match
    case (None,None) => "globally"
    case (Some(x),None) => s"$x->_"
    case (None, Some(x)) => s"_->$x"
    case (Some(x),Some(y)) => s"$x->$y"


