package ateams.backend

import ateams.syntax.Program.ASystem
import ateams.syntax.Program.Act
import caos.sos.SOS
import ateams.backend.Semantics.St
import ateams.syntax.Show
import ateams.backend.TypeCheck.getAllLocs
import ateams.backend.Semantics.Ctx
import ateams.syntax.Program.Proc
import ateams.syntax.Program.ProcName
import ateams.syntax.Program.Agent
import ateams.syntax.Program.SyncType
import ateams.syntax.Program
import ateams.syntax.Program.SyncType.Async
import ateams.backend.Semantics.getActName

object FindStructure:

  def apply(as:ASystem): String = 
    s"graph TD\n${
      {readSyncs(as).mkString("\n")}}\n${
      readAsync(as).mkString("\n")}\n${
      readGlobal(as).mkString("\n")
      }"
    

  // Find synchrounous interactions and represent them as solid edges
  def readSyncs(as:ASystem): Set[String] =
    val (_,edges,_,done) = SOS.traverseEdges(Semantics,St(as,Map()),1000)
    edges.flatMap(e => readEdge(e._2,as))

  private def readEdge(act:Act,as:ASystem): Set[String] =
    (act match
      case Act.In(a, from) =>  Set()
      case Act.Out(a, to) => Set()
      case Act.IO(a, from, to) => 
        for in <- from; out <- to yield
          // s"  ${ids(out)}([${fix(out)}]);\n" +
          // s"  ${ids(in)}([${fix(in)}]);\n" +
          // s"  ${ids(in)} -->|${fix(a)}| ${ids(out)};"
          s"  ${out}([${fix(out)}]);\n" +
          s"  ${in}([${fix(in)}]);\n" +
          s"  $in -->|$a| $out"
    ) // + act.toString

  // Find asynchronous interactions and represent them as dashed edges
  private def readAsync(as:ASystem): Set[String] =
    for (in,out,a) <- getAsyncs(as) if !(isSync(a,as))  yield
      s"  ${out}([${fix(out)}]);\n" +
      s"  ${in}([${fix(in)}]);\n" +
      s"  $in -.->|$a| $out"

  private def isSync(a:String,as:ASystem): Boolean =
    as.msgs.get(a) match
      case Some(Program.MsgInfo(_, Some(SyncType.Sync))) => true
      case _ => false
    

  private def getAsyncs(as: ASystem): Set[(String,String,String)] =
    val res = for (ag, p) <- as.main yield getAsync(p, Set())(using ag, Ctx(as.msgs,as.defs))
    res.foldLeft(Set[(String,String,String)]())(_++_)

  private def getAsync(p:Proc, done:Set[ProcName])
             (using self:Agent, ctx:Ctx): Set[(String,String,String)] =
      // Set((p.toString,"-","-")) ++ 
      (
      p match
        case Proc.End => Set()
        case Proc.ProcCall(p) if done(p) => Set()
        case Proc.ProcCall(p) => getAsync(ctx.defs(p),done+p)
        case Proc.Prefix(act, p) =>
          val rest = getAsync(p,done)
          val now = act match
            case Act.In(a, from) => for f<-from yield (f,self,a)
            case Act.Out(a, to) => for t<-to yield (self,t,a)
            case Act.IO(a, from, to) => for f<-from; t<-to yield (f,t,a)
          now++rest
        
        case Proc.Choice(p1, p2) =>
          getAsync(p1,done) ++ getAsync(p2,done) // could enrich one of the "done"s
        case Proc.Par(p1, p2) =>
          getAsync(p1,done) ++ getAsync(p2,done) // could enrich one of the "done"s
      )

  // Find global interactions and represent them as dotted edges
  private def readGlobal(as:ASystem): Set[String] =
    for act <- getGlobals(as, Set()) //if (isGlobal(act,Ctx(as.msgs,as.defs)))
    yield
      act match
        case Left((a,in)) =>
          s"  _global[[global]];\n" +
          s"  ${in}([${fix(in)}]);\n" +
          s"  $in ==>|$a| _global;"
        case Right((out,a)) =>
          s"  _global[[global]];\n" +
          s"  ${out}([${fix(out)}]);\n" +
          s"  _global ==>|$a| $out;"


  private def isGlobal(a:String,ctx:Ctx): Boolean =
    ctx.msgs.get(a) match
      case Some(Program.MsgInfo(_, Some(SyncType.Async(Program.LocInfo(false,false),_)))) => true
      case _ => false
  private def isGlobal(ma:Either[(String,String),(String,String)],ctx:Ctx): Boolean =
    val a = ma match
      case Left((_,a)) => isGlobal(a,ctx)
      case Right((a,_)) => isGlobal(a,ctx)
    a


  private def getGlobals(as: ASystem, done:Set[ProcName]): Set[Either[(String,String),(String,String)]] =
    val res = for (ag, p) <- as.main yield getGlobal(p, done)(using ag, Ctx(as.msgs,as.defs))
    res.foldLeft(Set[Either[(String,String),(String,String)]]())(_++_)
  private def getGlobal(p:Proc, done:Set[ProcName])(using self:Agent, ctx:Ctx): Set[Either[(String,String),(String,String)]] =
    p match
      case Proc.End => Set()
      case Proc.ProcCall(p) if done(p) => Set()
      case Proc.ProcCall(p) => getGlobal(ctx.defs(p), done+p)
      case Proc.Prefix(act, p) if isGlobal(getActName(act),ctx) => 
        val rest = getGlobal(p, done)
        val now = act match
          case Act.In(a, _) => Set(Left((a,self)))
          case Act.Out(a, _) => Set(Right((self,a)))
          case _ => Set()
        now++rest
      case Proc.Prefix(act, p) => getGlobal(p, done)
      case Proc.Choice(p1, p2) => getGlobal(p1, done) ++ getGlobal(p2, done)
      case Proc.Par(p1, p2) => getGlobal(p1, done) ++ getGlobal(p2, done)
    
    // val res = for (ag, p) <- as.main yield getGlobal(p, Set())(using ag, Ctx(as.msgs,as.defs))
    // res.foldLeft(Set[Either[(String,String),(String,String)]]())(_++_)
  

  // private def readGlobal(as:ASystem): Set[String] =
  //   for (act, MsgInfo(_, Some(Async(LocInfo(false,false))))) <- as.msgs
  //   yield
  //     s"  ${msgInfo.from}([${fix(msgInfo.from)}]);\n" +
  //     s"  ${msgInfo.to}([${fix(msgInfo.to)}]);\n" +
  //     s"  ${msgInfo.to}([${fix(msgInfo.to)}]);\n" +
  //     s"  ${msgInfo.from} -.->|${fix(m)}| ${msgInfo.to}"

  // type IDS = scala.collection.mutable.Map[Any,Int]
  // var i = 10
  // var _ids: Map[Any,Int] = Map()
  // private def ids(s:Any)(using _ids: IDS): Int =
  //     if _ids.contains(s) then
  //       _ids(s)
  //     else
  //       _ids+=s->i
  //       i+=1
  //       i-1


  private def fix(s:String): String = if s.startsWith("$$") then s.drop(2) else
      s"\" $s\""
      .replaceAll("<", "&lt;")
      .replaceAll(">", "&gt;")
      .replaceAll("\n","<br>")
    


        
