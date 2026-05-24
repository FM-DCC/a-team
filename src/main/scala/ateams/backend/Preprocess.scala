package ateams.backend

import ateams.syntax.Program.GProc
import ateams.syntax.Program.LProc
import ateams.syntax.Program.ASystem
import ateams.syntax.Program.MsgInfo
import ateams.syntax.Program.Agent
import ateams.syntax.Program.Proc
import ateams.syntax.Program.ProcName
import ateams.syntax.Program.GAct
import ateams.syntax.Program.LAct
import ateams.backend.Semantics.arit
import ateams.backend.Semantics.Ctx
import ateams.syntax.Program.SyncType
import ateams.syntax.Program.LocInfo
import ateams.syntax.Program.ActName
import ateams.syntax.Show

object Preprocess:

  //// all preprocesses ////
  def apply(sy:ASystem): ASystem =
    checkGuardedness(
    projectInits(
    projectProts(
    loadDefault(
      sy
    ))))

  ////// Preprocess: expand "default" synchronisation types ////

  def loadDefault(sys: ASystem): ASystem = 
    val mi = sys.msgs.getOrElse("default",MsgInfo(None,None))
    sys.copy(msgs =
      sys.msgs.map((k,v) => (k, v.copy(
        arity = v.arity.orElse(mi.arity).orElse(Some(MsgInfo.defaultArity)),
        st = v.st.orElse(mi.st).orElse(Some(MsgInfo.defaultST)),
      ))) // + ("tau" -> MsgInfo(None,None))
    )


  ///// Collect agents and processes /////

  def getProcCalls(ps:Set[GProc],done:Set[ProcName])(using sy:ASystem): Set[ProcName] =
    ps.headOption match
      case None => done
      case Some(p) => p match
        case Proc.End() => getProcCalls(ps-p,done)
        case Proc.ProcCall(p2) => 
          getProcCalls(ps-p+sy.prots.getOrElse(p2,sys.error(s"Protocol named $p2 not found.")),done+p2)
        case Proc.Prefix(act, p2) =>  getProcCalls(ps-p+p2,done)
        case Proc.Choice(p1, p2) =>  getProcCalls(ps-p+p1,getProcCalls(ps-p+p2,done))
        case Proc.Par(p1, p2) => getProcCalls(ps-p+p1,getProcCalls(ps-p+p2,done))
      
  def getAgents(sy: ASystem): Set[Agent] =
    sy.prots.values.toSet.flatMap(getAgents)

  def getAgents(p:GProc): Set[Agent] = p match
    case Proc.End() => Set()
    case Proc.ProcCall(p) => Set()
    case Proc.Prefix(act, p) => getAgents(act)++getAgents(p)
    case Proc.Choice(p1, p2) => getAgents(p1)++getAgents(p2)
    case Proc.Par(p1, p2) => getAgents(p1)++getAgents(p2)

  def getAgents(act:GAct): Set[Agent] = act match
    case GAct.In(a, from, to) => Set(to) //from+to
    case GAct.Out(a, from, to) => Set(from) // to+from
    case GAct.IO(a, from, to) => from++to
  
  // def collectProtoc(sy:ASystem): Set[GProc] = 
  //   for proc <- sy.main.values.toSet; prot <- collectProtoc(proc,sy)
  //   yield prot
    
  // def collectProtoc(proc:LProc,sy:ASystem): Set[GProc] =
  //   proc match
  //     case Proc.ProcCall(pn) => sy.prots.get(pn).toSet    
  //     case _ => Set()
    
  ///// Project to agents /////

  def projectProts(sy:ASystem): ASystem =
    val newProcs =
      for (pn,prot) <- sy.prots; ag<-getAgents(prot); proc <- projectProc(prot,ag)(using sy).toSet
      yield (mkPName(pn,ag), proc)
    sy.copy(defs = sy.defs ++ newProcs)


  // Project a single process - None means nothing was projected
  def projectProc(p:GProc,ag:Agent)(using sy:ASystem)
      : Option[LProc] = 
    p match
      case Proc.End() =>  Some(Proc.End())
      case Proc.ProcCall(p2) => Some(Proc.ProcCall(mkPName(p2,ag)))
      case Proc.Prefix(act, p2) => projectAct(act,ag) match
        case None => projectProc(p2,ag)
        case Some(lact) =>
          projectProc(p2,ag).map(pl => Proc.Prefix(lact,pl))
      case Proc.Choice(p1, p2) => (projectProc(p1,ag),projectProc(p2,ag)) match
        case (None,None) => None
        case (Some(x),None) => Some(x)
        case (None,Some(x)) => Some(x)
        case (Some(x),Some(y)) => Some(Proc.Choice(x,y))
      case Proc.Par(p1, p2) => (projectProc(p1,ag),projectProc(p2,ag)) match
        case (None,None) => None
        case (Some(x),None) => Some(x)
        case (None,Some(x)) => Some(x)
        case (Some(x),Some(y)) => Some(Proc.Par(x,y))
    
  def mkPName(pn:ProcName,a:Agent): ProcName = s"$pn$$$a"

  // project an action
  def projectAct(act:GAct,ag:Agent)(using sy:ASystem): Option[LAct] =
    act match
      case GAct.In(a, from, `ag`) => Some(LAct.In(a,mbFrom(from,a)))
      case GAct.Out(a, `ag`, to) => Some(LAct.Out(a,mbTo(to,a)))
      case GAct.IO(a, from, to) if from(ag) => Some(LAct.Out(a,mbTo(to,a)))
      case GAct.IO(a, from, to) if to(ag) => Some(LAct.In(a,mbFrom(from,a)))
      case _ => None
    
  def mbFrom(ag:Set[Agent],a:ActName)(using sy:ASystem): Set[Agent] =
    sy.msgs.get(a) match
      case Some(MsgInfo(_,Some(SyncType.Async(LocInfo(false,_), _)))) => Set()
      case _ => ag
  def mbTo(ag:Set[Agent],a:ActName)(using sy:ASystem): Set[Agent] =
    sy.msgs.get(a) match
      case Some(MsgInfo(_,Some(SyncType.Async(LocInfo(_,false), _)))) => Set()
      case _ => ag
    

  //// Project the INIT part ////

  def projectInits(sy:ASystem): ASystem =
    val ags = getAgents(sy)
    projectInits(sy,ags)

  def projectInits(sy:ASystem,ags:Set[Agent]): ASystem =
    def isProt(p:LProc): Boolean = p match
      case Proc.ProcCall(pn) => sy.prots.contains(pn)
      case _ => false
    val oldMain = sy.main.filterNot((ag,proc) => isProt(proc))
    val newMain = for (_,proc) <- sy.main.toSet
                      (ag,prot) <- expandProtoc(proc,ags,sy)
                      if prot match
                          case Proc.ProcCall(newName) =>
                            //println(s"CHECKING if $newName appears in ${sy.defs.mkString(",")}")
                            sy.defs.contains(newName)
                          case _ => true
              yield
                if sy.main.contains(ag) then
                  sys.error(s"Agent $ag appears in ${Show(proc)}, but it already exists.")
                (ag,prot)
    val newMainMap = newMain.toMap
    if newMainMap.size != newMain.size then
      val listPs = newMain.toList.map(_._1)
      val rep = listPs.filter(x => listPs.count(_==x)>1).toSet
      sys.error(s"Repeated agents in protocols: ${rep.mkString(",")}.")
    sy.copy(main = oldMain ++ newMain.toMap)
    
  def expandProtoc(proc:LProc,ags:Set[Agent],sy:ASystem): Set[(Agent,LProc)] =
    proc match
      case Proc.ProcCall(pn) if sy.prots.contains(pn) => 
        ags.map(ag => ag -> Proc.ProcCall(mkPName(pn,ag)))
      case _ => Set()


  /* 
  X = a->b:m1 . b->a:m2 . m3!a-b . X
  init X
  --
  X$a = m1!b . m2?b . m3!b . X$a
  X$b = m1?a . m2!a . X$b
  init X$a || X$b

  */

  ///// check guardedness ////
  def isGuarded[A](p:Proc[A]): Option[ProcName] = p match
    case Proc.End() => None
    case Proc.ProcCall(p) => Some(p)
    case Proc.Prefix(act, p) => None
    case Proc.Choice(p1, p2) => isGuarded(p1) orElse isGuarded(p2)
    case Proc.Par(p1, p2) => isGuarded(p1) orElse isGuarded(p2)

  def checkGuardedness(sy:ASystem): ASystem =
    for (pn,p) <- sy.defs if isGuarded(p).nonEmpty do
      sys.error(s"Definition of $pn is not guarded - call to ${isGuarded(p).get} is not prefixed.")
    sy
  
  