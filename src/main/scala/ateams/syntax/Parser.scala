package ateams.syntax

import cats.parse.Parser.*
import cats.parse.{LocationMap, Numbers, Parser as P, Parser0 as P0}
import ateams.syntax.Program.*
import Proc.*
import Buffer.*
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
    string("prot") *> sps *> prots.repSep(sps).map(joinASystems) |
    string("init") *> sps *> main

  lazy val notKw: P0[Unit] =
    not(string("acts")|string("proc")|string("init")|string("prot"))

  lazy val msg: P[ASystem] =
    (((notKw.with1 *> varName) <* sps) ~
        ((char(':') *> sps *> msgInfo) |
          char(';').as[MsgInfo](MsgInfo(None,None)))
    ).map((v,i) => ASystem(Map(v->i),Map(),Map(),Map()))
  lazy val defs: P[ASystem] =
    (notKw.with1 *> (procName <* sps <* char('=') <* sps) ~ proc)
      .map((v, i) => ASystem(Map(),Map(v -> i),Map(), Map()))
  lazy val prots: P[ASystem] =
    (notKw.with1 *> (procName <* sps <* char('=') <* sps) ~ prot)
      .map((v, i) => ASystem(Map(),Map(),Map(v -> i), Map()))
  lazy val main: P[ASystem] =
    namedProc.repSep(sps *> string("||") *> sps)
      .map(l => ASystem(Map(),Map(),Map(),
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

  lazy val procOrProt: P[LProc] =
    proc.backtrack | prot.as(End[LAct]())

  // Processes
  lazy val proc: P[LProc] = P.recursive(more =>
    procSum(more).repSep(sps *> char('|') <* sps)
      .map(l => l.toList.tail.foldLeft(l.head)((t1, t2) => Par(t1, t2)))
  )
  lazy val namedProc:P[(Option[String],LProc)] =
    ((varName <* sps) ~ namedProcCont).map((v,cont) => cont(v)) |
    procName.map(p => (None,ProcCall(p)))

  lazy val namedProcCont:P[String => (Option[String],LProc)] =
    char(':') *> sps *> proc.map(p => (str:String) => (Some(str),p))

  private def procSum(more:P[LProc]): P[LProc] =
    (procSeq(more)<*sps).repSep(char('+') <* sps)
      .map(l=>l.toList.tail.foldLeft(l.head)((t1,t2)=>Choice(t1,t2)))

  private def procSeq(more:P[LProc]): P[LProc] = P.recursive(t2 =>
    end | procCall | pref(t2) | char('(')*>more.surroundedBy(sps)<*char(')')
  )

  private def end[A] =
    char('0').as(End[A]())

  private def procCall[A]= // : P[LProc] =
    procName.map(ProcCall[A].apply)

  private def pref(t2:P[LProc]): P[LProc] =
    ((laction <* sps) ~ ((char('.') *> sps *> t2)?))
      .map(x => Prefix(x._1,x._2.getOrElse(End())))

  private def laction: P[LAct] =
    ((varName <* sps) ~ inOut.?).map {
      case (v, Some(io)) => io(v)
      case (v, None) => LAct.Internal(v)
    }
//    string("tau").as(Act.IO("tau",Set(),Set())) |
//    ((varName <* sps) ~ inOut).map(vi => vi._2(vi._1))

  private def inOut: P[String => LAct] =
    char('?') *> anyName.repSep0(char(',')).map(lst => (a:String) => LAct.In(a,lst.toList.toSet)) |
    char('!') *> anyName.repSep0(char(',')).map(lst => (a:String) => LAct.Out(a,lst.toList.toSet))

  // Protocols (too much copy paste, but difficult to reuse)
  lazy val prot: P[GProc] = P.recursive(more =>
    protSum(more).repSep(sps *> char('|') <* sps)
      .map(l => l.toList.tail.foldLeft(l.head)((t1, t2) => Par(t1, t2)))
  )
  lazy val namedProt:P[(Option[String],GProc)] =
    ((varName <* sps) ~ namedProtCont).map((v,cont) => cont(v)) |
    procName.map(p => (None,ProcCall(p)))

  lazy val namedProtCont:P[String => (Option[String],GProc)] =
    char(':') *> sps *> prot.map(p => (str:String) => (Some(str),p))

  private def protSum(more:P[GProc]): P[GProc] =
    (protSeq(more)<*sps).repSep(char('+') <* sps)
      .map(l=>l.toList.tail.foldLeft(l.head)((t1,t2)=>Choice(t1,t2)))

  private def protSeq(more:P[GProc]): P[GProc] = P.recursive(t2 =>
    end | procCall | gpref(t2) | char('(')*>more.surroundedBy(sps)<*char(')')
  )

  private def gpref(t2:P[GProc]): P[GProc] =
    ((gaction <* sps) ~ ((char('.') *> sps *> t2)?))
      .map(x => Prefix(x._1,x._2.getOrElse(End())))

  // e.g.: act!fr1,fr2-to1,to2
  // BEFORE: act!to1,to2
  private def gaction: P[GAct] =
    ((varName <* sps) ~ ginOut.?).map {
      case (v, Some(io)) => io(v)
      case (v, None) => GAct.IO(v,Set(),Set()) // to check
    }

  private def ginOut: P[String => GAct] =
    (char('?') *> sps *> fromTo)
      .map(lst => (a:String) => 
        if lst._2.size>1
        then sys.error(s"Input can only have one target in \"$a?${lst._1.mkString(",")}-${lst._2.mkString(",")}\"")
        if lst._2.isEmpty && lst._1.size != 1
        then sys.error(s"Input can only have one target in \"$a?${lst._1.mkString(",")}\"")
        if lst._2.isEmpty
        then GAct.In(a,Set(),lst._1.head)
        else GAct.In(a,lst._1,lst._2.head)
        ) |
    (char('!') *> sps *> fromTo)
      .map(lst => (a:String) =>
        if lst._1.size>1
        then sys.error(s"Output can only have one target in \"$a?${lst._1.mkString(",")}-${lst._2.mkString(",")}\"")
        else GAct.Out(a,lst._1.head,lst._2)) |
    (char(',') *> sps *> (anyName.repSep(char(',')) ~
        (sps *> string("->") *> sps *> intercCont)))
      .map(rest => (fr:String) => GAct.IO(rest._2._2,rest._1.toList.toSet+fr,rest._2._1)) |
    (string("->") *> sps *> intercCont)
      .map(intrc => (fr:String) => GAct.IO(intrc._2,Set(fr),intrc._1)) 

  private def fromTo: P[(Set[String],Set[String])] =
    (anyName.repSep(char(',')) ~ (char('-') *> anyName.repSep(char(','))).?)
      .map((x,y) => (x.toList.toSet,y.toList.map(_.toList).flatten.toSet))

  private def intercCont: P[(Set[String],String)] = 
    (anyName.repSep(char(',')) ~ (sps *> char(':') *> sps *> anyName))
      .map((l1,n) => (l1.toList.toSet,n))




//// Protocol


/* 
  def program: Parser[Protocol] =
    opt(choreography) ^^ {case c => c.getOrElse(End)}
 
  def par[A](parser: Parser[A]): Parser[A] = "(" ~> parser <~ ")"
  
  def agent: Parser[Agent] = id ^^ Agent.apply
  def agents: Parser[List[Agent]] = repsep(agent, ",")
  
  def message:Parser[ActName] = 
    ":" ~> id //rep1sep(id,",") ^^ {case ms => Msg(ms)}
  
  /**
   * A choreography expression
   * - Left associativity: ;,+,||
   * - Precedence: i,(c)>*>;>||>+
   *
   * @return
   */
  def choreography: Parser[Protocol] =
    maybeParallel ~ opt(choice) ^^ {
      case mb ~ Some(ch) => ch(mb)
      case mb ~ _ => mb
    }

  def choice: Parser[Protocol => Protocol] =
    "+" ~ maybeParallel ~ opt(choice) ^^ {
      case _ ~ mc ~ Some(more) => (lhs:Protocol) => more(Choice(lhs,mc))
      case _ ~ mc ~ _          => (lhs:Protocol) => Choice(lhs,mc)
    } |
    "[+]" ~ maybeParallel ~ opt(choice) ^^ {
      case _ ~ mc ~ Some(more) => (lhs:Protocol) => more(DChoice(lhs,mc))
      case _ ~ mc ~ _          => (lhs:Protocol) => DChoice(lhs,mc)
    }

  def maybeParallel: Parser[Protocol] =
    maybeSequence ~ opt(parallel) ^^ {
      case lhs ~ Some(pll) => pll(lhs)
      case lhs ~ None => lhs
    }

  def parallel: Parser[Protocol => Protocol] =
    "||" ~ maybeSequence ~ opt(parallel) ^^ {
      case _ ~ ms ~ Some(more) => (lhs: Protocol) => more(Par(lhs, ms))
      case _ ~ ms ~ _    => (lhs: Protocol) => Par(lhs, ms)
    }

  def maybeSequence: Parser[Protocol] =
    atomChoreography ~ opt(sequence) ^^ {
      case lhs ~ Some(seq) => seq(lhs)
      case lhs ~ _ => lhs
    }

  def sequence: Parser[Protocol => Protocol] =
    (";"|".") ~ atomChoreography ~ opt(sequence) ^^ {
      case _ ~ seq ~ Some(more) => (lhs: Protocol) => more(Seq(lhs, seq))
      case _ ~ seq ~ _          => (lhs: Protocol) => Seq(lhs, seq)
    }

  def atomChoreography: Parser[Protocol] =
    literal ~ opt("*") ^^ {
      case lit ~ l => if l.isDefined then Loop(lit) else lit
    }

  def literal: Parser[Protocol] =
    "("~>choreography<~")" |
    "1" ^^^ End |
//    agent ~ ("\\?|!|(->)".r) ~ agent ~ opt(message) ^^ {
//      case a ~ "?" ~ b ~ ms =>   In(  a, b, ms.getOrElse(Msg(List())))
//      case a ~ "!" ~ b ~ ms =>   Out( a, b, ms.getOrElse(Msg(List())))
//      case a ~ _   ~ b ~ ms =>   Send(List(a), List(b), ms.getOrElse(Msg(List())))
    agents ~ opt(("\\?|!|(->)".r) ~ agents) ~ opt(message) ^^ {
      case a ~ Some("?" ~ b) ~ ms => // sys.error("unsupported a?b") //In(a, b, ms.getOrElse(Msg(List())))
        (for aa<-a; bb<-b yield In(aa,bb,ms.getOrElse(Msg(List())))).fold(End)(_||_)
      case a ~ Some("!" ~ b) ~ ms => // sys.error("unsupported a!b") //Out(a, b, ms.getOrElse(Msg(List())))
        (for aa<-a; bb<-b yield Out(aa,bb,ms.getOrElse(Msg(List())))).fold(End)(_||_)
      case a ~ Some(_ ~ b) ~ ms => Send(a, b, ms.getOrElse(Msg(List())))
      case a ~ None ~ ms => a.map(x=>IO(x,ms.getOrElse(Msg(List())))).fold(End)(_||_)
    }

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
*/