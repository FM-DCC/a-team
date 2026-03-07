package ateams.syntax

import cats.parse.Parser.*
import cats.parse.{LocationMap, Numbers, Parser as P, Parser0 as P0}
import ateams.syntax.Program.*
import cats.data.NonEmptyList

import scala.sys.error

object Parser :

  /** Parse a command  */
  def parseProgram(str:String):ASystem =
    pp(program,str) match {
      case Left(e) => error(e)
      case Right(c) => c
    }

  /** Applies a parser to a string, and prettifies the error message */
  private def pp[A](parser:P[A], str:String): Either[String,A] =
    parser.parseAll(str) match
      case Left(e) => Left(prettyError(str,e))
      case Right(x) => Right(x)

  /** Prettifies an error message */
  private def prettyError(str:String, err:Error): String =
    val loc = LocationMap(str)
    val pos = loc.toLineCol(err.failedAtOffset) match
      case Some((x,y)) =>
        s"""at ($x,$y):
           |<pre>${loc.getLine(x).getOrElse("-")}</br>${("-" * y)+"^\n"}</pre>""".stripMargin
      case _ => ""
    s"${pos}expected: ${err.expected.toList.mkString(", ")}\noffsets: ${
      err.failedAtOffset};${err.offsets.toList.mkString(",")}"

  // Simple parsers for spaces and comments
  /** Parser for a sequence of spaces or comments */
  private val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  private val comment: P[Unit] = string("//") *> P.charWhere(_!='\n').rep0.void
  private val sps: P0[Unit] = (whitespace | comment).rep0.void

  // Parsing smaller tokens
  private def alphaDigit: P[Char] =
    P.charIn('A' to 'Z') | P.charIn('a' to 'z') | P.charIn('0' to '9') | P.charIn('_')
  private def varName: P[String] =
    (charIn('a' to 'z') ~ alphaDigit.rep0).string
  private def procName: P[String] =
    (charIn('A' to 'Z') ~ alphaDigit.rep0).string
  private def anyName: P[String] =
    ((charIn('A' to 'Z') | P.charIn('a' to 'z')) ~ alphaDigit.rep0).string
  private def symbols: P[String] =
    // symbols starting with "--" are meant for syntactic sugar of arrows, and ignored as symbols of terms
    P.not(string("--")).with1 *>
    oneOf("+-><!%/*=|&".toList.map(char)).rep.string
  private lazy val intP: P[Int] =
    Numbers.digits.map(_.toInt)

  import scala.language.postfixOps

  /** A program is a command with possible spaces or comments around. */
  private def program: P[ASystem] =
    (sps.with1 *> oneProgram.repSep(sps) <* sps)
      .map(joinASystems)


  private def joinASystems(l:NonEmptyList[ASystem]): ASystem =
    l.tail.foldLeft(l.head)(_++_)
  private def joinASystems(l:List[ASystem]): ASystem = l match
    case hd::tail => tail.toList.foldLeft(hd)(_++_)
    case Nil => ASystem.default

  private def oneProgram: P[ASystem] =
    string("acts") *> sps *> msg.repSep(sps).map(joinASystems) |
    string("proc") *> sps *> defs.repSep(sps).map(joinASystems) |
    string("init") *> sps *> main

  lazy val notKw: P0[Unit] =
    not(string("acts")|string("proc")|string("init"))

  lazy val msg: P[ASystem] =
    (((notKw.with1 *> varName) <* sps) ~
        ((char(':') *> sps *> msgInfo) |
          char(';').as[MsgInfo](MsgInfo(None,None)))
    ).map((v,i) => ASystem(Map(v->i),Map(),Map()))
  lazy val defs: P[ASystem] =
    (notKw.with1 *> (procName <* sps <* char('=') <* sps) ~ proc)
      .map((v, i) => ASystem(Map(),Map(v -> i), Map()))
  lazy val main: P[ASystem] =
    namedProc.repSep(sps *> string("||") *> sps)
      .map(l => ASystem(Map(),Map(),
//        l.toList.zipWithIndex.map((x,i)=>(Show(x)+"_"+i) -> x).toMap))
        l.toList.zipWithIndex.map((x,i)=> x._1.getOrElse(i.toString) -> x._2).toMap))


  // Message declarations
  lazy val msgInfo: P[MsgInfo] =
    (msgMod.repSep(sps *> char(',') *> sps) <* char(';'))
      .map(l => l.foldLeft(MsgInfo(None,None))((mi,mod) => mod(mi)))

  lazy val intrv: P[(Int,Option[Int])] =
    ((intP <* sps) ~ (char('.') *> char('.').rep *>
      (intP.map(Some(_))|char('*').as(None))).?)
      .map((start,end) => (start,end.getOrElse(Some(start))))

  lazy val msgMod: P[MsgInfo => MsgInfo] =
    ((intrv <* sps <* string("->") <* sps) ~ intrv)
      .map((i1,i2) => (mi:MsgInfo) => mi.copy(arity = Some(i1->i2))) |
    string("sync").as((mi:MsgInfo) => mi.copy(st = Some(SyncType.Sync))) |
    (string("fifo") *> sps *> optLoc.?)
      .map(l=> (mi:MsgInfo) =>
        mi.copy(st = Some(SyncType.Async(l.getOrElse(LocInfo(false,true)),Fifo())))) |
    (string("unsorted") *> sps *> optLoc.?)
      .map(l=> (mi:MsgInfo) =>
        mi.copy(st = Some(SyncType.Async(l.getOrElse(LocInfo(false,true)),Unsorted())))) |
    (string("prioqueue") *> sps *> optLoc.?)
      .map(l=> (mi:MsgInfo) =>
        mi.copy(st = Some(SyncType.Async(l.getOrElse(LocInfo(false,true)),PrioQueue()))))

  lazy val optLoc: P[LocInfo] =
    char('@') *> sps *> (
      string("snd-rcv").as(LocInfo(true,true)) |
      string("snd").as(LocInfo(true,false)) |
      string("rcv").as(LocInfo(false,true)) |
      string("global").as(LocInfo(false,false))
    )

  // Processes
  lazy val proc: P[Proc] = P.recursive(more =>
    procSum(more).repSep(sps *> char('|') <* sps)
      .map(l => l.toList.tail.foldLeft(l.head)((t1, t2) => Proc.Par(t1, t2)))
  )
  lazy val namedProc:P[(Option[String],Proc)] =
    ((varName <* sps) ~ namedProcCont).map((v,cont) => cont(v)) |
    procName.map(p => (None,Proc.ProcCall(p)))

  lazy val namedProcCont:P[String => (Option[String],Proc)] =
    char(':') *> sps *> proc.map(p => (str:String) => (Some(str),p))

  private def procSum(more:P[Proc]): P[Proc] =
    (procSeq(more)<*sps).repSep(char('+') <* sps)
      .map(l=>l.toList.tail.foldLeft(l.head)((t1,t2)=>Proc.Choice(t1,t2)))

  private def procSeq(more:P[Proc]): P[Proc] = P.recursive(t2 =>
    end | procCall | pref(t2) | char('(')*>more.surroundedBy(sps)<*char(')')
  )

  private def end: P[Proc] =
    char('0').as(Proc.End)

  private def procCall: P[Proc] =
    procName.map(Proc.ProcCall.apply)

  private def pref(t2:P[Proc]): P[Proc] =
    ((action <* sps) ~ ((char('.') *> sps *> t2)?))
      .map(x => Proc.Prefix(x._1,x._2.getOrElse(Proc.End)))

  private def action: P[Act] =
    ((varName <* sps) ~ inOut.?).map {
      case (v, Some(io)) => io(v)
      case (v, None) => Act.IO(v,Set(),Set())
    }
//    string("tau").as(Act.IO("tau",Set(),Set())) |
//    ((varName <* sps) ~ inOut).map(vi => vi._2(vi._1))

  private def inOut: P[String => Act] =
    char('?') *> anyName.repSep0(char(',')).map(lst => (a:String) => Act.In(a,lst.toList.toSet)) |
    char('!') *> anyName.repSep0(char(',')).map(lst => (a:String) => Act.Out(a,lst.toList.toSet))

  //  private def system: P[CCSSystem] =
//    string("let") *> sps *>
//    ((defn.repSep0(sps)<*sps<*string("in")<*sps).with1 ~ term)
//      .map((x,y)=>CCSSystem(x.toMap,y,None))
//  private def defn:P[(String,Term)] =
//    (procName <* char('=').surroundedBy(sps)) ~
//      (term <* sps <* char(';'))
//
//  private def term: P[Term] = P.recursive(more =>
//    termSum(more).repSep(sps *> char('|') <* sps)
//      .map(l => l.toList.tail.foldLeft(l.head)((t1, t2) => Par(t1, t2)))
//  )
//  private def termSum(more:P[Term]): P[Term] =
//    (termSeq(more)<*sps).repSep(char('+') <* sps)
//      .map(l=>l.toList.tail.foldLeft(l.head)((t1,t2)=>Choice(t1,t2)))
//
//  private def termSeq(more:P[Term]): P[Term] = P.recursive(t2 =>
//    end | proc | pref(t2) | char('(')*>more.surroundedBy(sps)<*char(')')
//  )
//
//  private def end: P[Term] =
//    char('0').as(End)
//
//  private def proc: P[Term] =
//    procName.map(ProcCall.apply)
//
//  private def pref(t2:P[Term]): P[Term] =
//    ((varName <* sps) ~ ((char('.') *> t2)?))
//      .map(x => Prefix(x._1,x._2.getOrElse(End)))


  //////////////////////////////
  // Examples and experiments //
  //////////////////////////////
  object Examples:
    val ex1 =
      """x:=28; while(x>1) do x:=x-1"""
