package ateams.syntax

import ateams.syntax.Program.*
import ateams.backend.Semantics.*

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

  def showDefs(ds: Map[String,Proc]): String =
    (for (nm,p) <- ds yield s"  $nm := ${apply(p)}")
      .mkString("\n")
  def showMain(m: Map[String,Proc]): String =
    (for (nm,p) <- m yield s"$nm: ${apply(p)}")
      .mkString(" || ")
  def showIntrv(intr: Intrv): String = intr._2 match
    case Some(n) if intr._1==n => n.toString
    case Some(n) => s"${intr._1}..$n"
    case None => s"${intr._1}..∞"

  def apply(p: Proc): String = p match
    case Proc.End => "skip"
    case Proc.ProcCall(p) => p
    case Proc.Prefix(act, Proc.End) => apply(act)
    case Proc.Prefix(act, t) => s"${apply(act)}.${applyP(t)}"
    case Proc.Choice(t1, t2) => s"${applyP(t1)}+${applyP(t2)}"
    case Proc.Par(t1, t2) => s"${applyP(t1)} | ${applyP(t2)}"

  private def applyP(p: Proc): String = p match
    case _:(Proc.End.type|Proc.ProcCall|Proc.Prefix) => apply(p)
    case _ => s"(${apply(p)})"

  def apply(a:Act): String = a match
    case Act.In(s,from) => s"$s?${from.mkString(",")}"
    case Act.Out(s,to) => s"$s!${to.mkString(",")}"
    case Act.IO("tau",_,_) => s"τ"
    case Act.IO(s,from,to) if from.isEmpty && to.isEmpty => s
    case Act.IO(s,from,to) => s"${agSet(from)}→${agSet(to)}:$s"
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
    case PrioQueue(q) => s"[${q.toList.sorted.mkString(",")}]"


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





  /** Converts the main term into a mermaid diagram reflecting its structure. */
//  def mermaid(s:CCSSystem): String = "graph TD\n" +
//    s"  style ${s.main.hashCode()} fill:#ffe177,stroke:#6a6722,stroke-width:4px;\n" +
//    (term2merm(s.defs) ++
//      term2merm(s.main)).mkString("\n")
//
//      /** Builds nodes and arcs, using a set structure to avoid repetition. */
//  private def term2merm(e: Term): Set[String] = e match
//    case Term.End => Set(s"  ${e.hashCode()}([\"0\"])")
//    case Term.ProcCall(p) => Set(s"  ${e.hashCode()}([\"$p\"])")
//    case Term.Prefix(act, t) => term2merm(t) ++
//      Set(s"  ${e.hashCode()} -->|action| ${act.hashCode()}",
//          s"  ${e.hashCode()} -->|rest| ${t.hashCode()}",
//          s"  ${e.hashCode()}([\"${apply(e)}\"])",
//          s"  ${act.hashCode()}([\"$act\"])"
//        )
//    case Term.Choice(t1, t2) =>
//      term2merm(t1) ++ term2merm(t2) ++
//      Set(s"  ${e.hashCode()} -->|option 1| ${t1.hashCode()}",
//          s"  ${e.hashCode()} -->|option 2| ${t2.hashCode()}",
//          s"  ${e.hashCode()}([\"${apply(e)}\"])")
//    case Term.Par(t1, t2) =>
//      term2merm(t1) ++ term2merm(t2) ++
//      Set(s"  ${e.hashCode()} -->|par 1| ${t1.hashCode()}",
//          s"  ${e.hashCode()} -->|par 2| ${t2.hashCode()}",
//          s"  ${e.hashCode()}([\"${apply(e)}\"])")
//
//  /** Builds the nodes and arcs of the diagram of the definitions */
//  private def term2merm(defs: Map[String,Term]): Set[String] =
//    defs.flatMap( (p,t) =>
//      term2merm(t) +
//      s"  ${ProcCall(p).hashCode()}([\"$p\"])" +
//      s"  ${ProcCall(p).hashCode()} -->|definition| ${t.hashCode}"
//    ).toSet
