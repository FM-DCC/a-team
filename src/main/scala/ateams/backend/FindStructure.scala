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

object FindStructure:

  def apply(as:ASystem): String = 
    s"graph TD\n${{readSyncs(as).mkString("\n")}}\n" + readAsync(as).mkString("\n")

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
    


        
