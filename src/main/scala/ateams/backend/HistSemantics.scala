package ateams.backend

import ateams.syntax.Program.GAct
import ateams.backend.Semantics.St
import HistSemantics.HSt
import caos.sos.SOS
import ateams.syntax.Program.ASystem
import ateams.syntax.Show
import ateams.syntax.Program.MsgInfo
import ateams.syntax.Program.SyncType
import ateams.syntax.Program.LocInfo
import ateams.syntax.Program.Agent

object HistSemantics extends SOS[GAct,HSt]:
  case class HSt(st: St,hist: List[(St,GAct)]):
    override def toString: String = Show(st)


  /** What are the set of possible evolutions (label and new state) */
  override def next[A>:GAct](hst: HSt): Set[(A, HSt)] =
    for (a,st2) <- Semantics.next(hst._1) yield
      (a,HSt(st2,(hst._1->a) :: hst._2))

  def initial(as:ASystem) = HSt(St(as,Map()),Nil)

  def hstToMermaid(hst:HSt): String =
    s"""sequenceDiagram
    autonumber
    ${participants(hst)}

    ${arrows(hst)}"""

  def participants(hst:HSt): String =
    val ps = (hst._2.map(_._2)).toSet.flatMap{
      case GAct.In(_, froms, to) => froms + to
      case GAct.Out(_, from, tos) => tos + from
      case GAct.IO(_, froms, tos) => froms ++ tos
      case GAct.Internal(_,ag) => Set(ag)
    }
    ps.toList.sorted.map(p => s"participant a${p.hashCode} as $p").mkString("\n    ")

  def arrows(hst:HSt): String =
    val lines = for (_,a) <- hst._2.reverse yield
      val ls = a match
        case GAct.In(a, from, to) =>
          for bf <- findBuffer(from,Set(to),hst._1.msgs.get(a),true).toList yield
            s"    ${bf._1} ->> a${to.hashCode()}: $a${bf._2}"
        case GAct.Out(a, from, to) =>
          for bf <- findBuffer(Set(from),to,hst._1.msgs.get(a),false).toList yield
            s"    a${from.hashCode()} ->> ${bf._1}: $a${bf._2}"
        case GAct.Internal("tau",ag) => 
          List(s"    Note over a${ag.hashCode}: 𝛕")
        case GAct.Internal(a,ag) => 
          List(s"    Note over a${ag.hashCode}: $a")
        case GAct.IO(a, from, to) =>
          if to.isEmpty then
            for f<-from.toList yield s"    a${f.hashCode} ->> buffer: $a"
          else if from.isEmpty then
            for t<-to.toList yield s"    buffer ->> a${t.hashCode()}: $a"
          else
            for (f <- from.toList; t <- to.toList) yield
              s"    a${f.hashCode} ->> a${t.hashCode()}: $a"
        if ls.size>1 then "rect rgb(0, 0, 255, .1)"::ls:::List("end") else ls
          
    //println(lines)
    lines.flatten.mkString("\n")
      
  def findBuffer(from:Set[Agent], to:Set[Agent], mi: Option[MsgInfo], in: Boolean)
      : Set[(String,String)] =
    mi match
      case None => sys.error("Searching for a buffer but no action found")
      case Some(mi: MsgInfo) => mi.st.match
        case Some(SyncType.Sync) => sys.error("Searching for a buffer of a sync action")
        case Some(SyncType.Internal) => sys.error("Searching for a buffer of an internal action")
        case None => sys.error("Searching for a buffer but no type found")
        case Some(SyncType.Async(where:LocInfo, _)) => where match
          case LocInfo(true, true)  => for f<-from; t<-to yield s"[$f-$t]" -> ""
          case LocInfo(true, false) => for f<-from yield s"[$f-_]" -> (
            if !in && mi.arity.isDefined && mi.arity.get._2._2.getOrElse(1) != 1 
            then s" (x${mi.arity.get._2._2.get})" else "")
          case LocInfo(false, true) => for t<-to yield s"[_-$t]" -> (
            if in && mi.arity.isDefined && mi.arity.get._1._2.getOrElse(1) != 1 
            then s" (x${mi.arity.get._1._2.get})" else "")
          case LocInfo(false,false) => Set(s"[global]" -> "")
