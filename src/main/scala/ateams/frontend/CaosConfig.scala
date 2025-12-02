package ateams.frontend

import caos.frontend.Configurator.*
import caos.frontend.{Configurator, Documentation}
import caos.view.*
import ateams.backend.*
import ateams.syntax.Program.{ASystem, preProcess}
import ateams.syntax.{Program, Show}
import ateams.backend.Semantics.St
import caos.sos.SOS

/** Object used to configure which analysis appear in the browser */
object CaosConfig extends Configurator[ASystem]:
  val name = "<span class=\"ateam\">A-Team</span> – Animator of Team Automata with asynchronous communication"
  override val languageName: String = "Input program"

  /** Parser, converting a string into a System in A-Teams */
  val parser =
    ateams.syntax.Parser.parseProgram
      .andThen(preProcess)

  /** Examples of programs that the user can choose from. The first is the default one. */
  val examples = List(
//    "ex0"
//      -> "acts\n  default: fifo, 1->1;\n  coin; coffee;\n\nproc\n CM = coin!cs.coffee?.CM\n CS = coin?.coffee!.CS\nin\n CM||cs:CS"
//      -> "Experiment",
    "coffee-sync"
      -> "acts\n  default: sync, 1->1;\n  coin; coffee;\n  pub: 1->0;\n  // other supported examples:\n  // fifo\n  // unsorted\n  // fifo @ rcv,\n  // fifo @ snd\n  // fifo @ snd-rcv\n  // fifo @ global\n  // 1..3 -> 4...5\n  // 1 -> 0..*\n\nproc\n CM = coin!.tau.coffee?.CM\n CS = pub!.coin?.coffee!.CS\ninit\n CM||CS"
      -> "Simple coffee with synchronous channels",
    "coffee-async"
      -> "acts\n  default: fifo, 1->1;\n  coin; coffee;\n  pub: 1->0;\n\nproc\n CM = coin!cs.coffee?.CM\n CS = pub!.coin?.coffee!cm.CS\ninit\n cm:CM||cs:CS"
      -> "Asynchronous version of the coffee machine with FIFO channels",
    "coffee-async-off"
      -> "acts\n  default: fifo@rcv, 1->1;\n  coin; coffee;\n\nproc\n // Coffee machine may turn off\n CM = coin!cs.coffee?.(off+CM)\n CS = coin?.coffee!cm.CS\n // start with a coin\n CM2 = coin!cs.CM\ninit\n cm:CM2||cs:CS"
      -> "Variation of the asynchronous coffee machine with an extra coin and a terminating option, to create orphan messages.",
    "race-sync"
      -> "acts\n  start:  1->2, sync;\n  finish: 1->1, sync;\nproc\n Ctr = start!.finish?.finish?.Ctr\n R = start?.finish!.R\ninit\n Ctr || R || R"
      -> "Synchronous runner example, without internal actions",
    "race@rcv"
      -> "acts\n  start:  1->2, fifo@rcv;\n  finish: 2->1, fifo@rcv;\nproc\n Ctr = start!r1,r2.finish?.Ctr\n R = start?.finish!c.R\ninit\n c:Ctr || r1:R || r2:R"
      -> "Race async - both actions send asynchronously, with a buffer for each receiver. This results in more states (10) than the synchronous version (2).",
    "race@snd"
      -> "acts\n  start:  1->2, fifo@snd;\n  finish: 2->1, fifo@snd;\nproc\n Ctr = start!.finish?r1,r2.Ctr\n R = start?c.finish!.R\ninit\n c:Ctr || r1:R || r2:R"
      -> "Race async - both actions send asynchronously, with a buffer for each sender. This is problematic because a runner can start, finish, and start again, consuming a start message that was meant to the other runner (causing a deadlock).",
    "race@global"
      -> "acts\n  start:  1->2, fifo@global;\n  finish: 2->1, fifo@global;\nproc\n Ctr = start!.finish?.Ctr\n R = start?.finish!.R\ninit\n c:Ctr || r1:R || r2:R"
      -> "Race async - both actions send asynchronously, with a single shared FIFO buffer for eveyone. This results in more states (9) than the synchronous version (2), and one less than the receiver, and allows a runner to start twice before the other starts.",
    "race@unbounded"
      -> "acts\n  start:  1->2, sync;\n  rest:   1->2, fifo@rcv;\n  finish: 2->1, fifo@rcv;\nproc\n Ctr = start!.finish?.Ctr\n     + rest!r1,r2.Ctr\n R = start?.finish!c.R\n   + rest?.R\ninit\n c:Ctr || r1:R || r2:R"
      -> "Variation of the race example with an unbounded fifo growing forever.",
    "race@errors"
      -> "acts start:  1->2, sync;\n     finish: 2->1, fifo@snd;\nproc Ctr = start!.finish?r1.Ctr\n\t\t R = start?.finish!c.R\ninit c:Ctr || r1:R || r2:R"
      -> "Race variation with type errors (ill-formed).",
    "healthcare-sync"
      -> "acts\n  default: fifo@snd-rcv;\n  fndCor; assRes; perFunc; mkPropRHS; gvCrite; proAcc; chkAut; rptAut; chkPol; confChkPol; proCriteEV; confCriteEV; seekAcc; forAcc; perInvs; rptInvs; decAcc; grntAcc; manVer; chkAutAcc; tecAss; rptTec; tcAss; rtTec; rprTec; sndPro; takAct; rptMon;\n\nproc\n  GDH =   fndCor!rC. \n          assRes!rCOO. \n          perFunc!rCOO. \n          Loop1GDH  | Loop3GDH | Loop5GDH\n  Loop1GDH = proAcc?rCOO. Loop1GDH\n  Loop3GDH = chkPol?rCOO. confChkPol!rCOO. Loop3GDH\n  Loop5GDH = seekAcc?lHA. forAcc!rCOO.  \n          (Loop5GDH\n           +\n           Loop51GDH)\n  Loop51GDH= (sndPro?rCOO.\n              takAct!lHA.\n              Loop5GDH\n              +\n              rptMon?rCOO.\n              takAct!lHA.\n              Loop5GDH)\n\n  RC =  fndCor?gDH. \n        mkPropRHS?rCOO. \n        gvCrite!rHS.\n        Loop4RC\n  Loop4RC = proCriteEV?oTAM. confCriteEV!oTAM. Loop4RC\n\n\n  RCOO = assRes?gDH. \n         perFunc?gDH. \n         mkPropRHS!rC. \n         Loop1RCOO | Loop2RCOO | Loop3RCOO | Loop5RCOO\n  Loop1RCOO = proAcc!gDH. Loop1RCOO\n  Loop2RCOO = chkAut!aC. rptAut?aC. Loop2RCOO\n  Loop3RCOO = chkPol!gDH. confChkPol?gDH. Loop3RCOO\n  Loop5RCOO = forAcc?gDH. perInvs!hAS. rptInvs?hAS.\n          ((decAcc!lHA. Loop5RCOO)\n           +\n           (grntAcc!lHA. manVer!oTA. Loop51RCOO +\n            manVer!oTA. grntAcc!lHA. Loop51RCOO))\n  Loop51RCOO = rprTec?oTAM.\n          ( sndPro!gDH. Loop5RCOO\n            +\n            rptMon!gDH. Loop5RCOO)\n\n  RHS = gvCrite?rC\n\n  AC = chkAut?rCOO. rptAut!rCOO. AC\n\n  OTAM = Loop4OTAM | Loop5OTAM\n  Loop4OTAM = proCriteEV!rC. confCriteEV?rC. Loop4OTAM\n  Loop5OTAM = chkAutAcc?oTA.\n     tecAss!eV. rptTec?eV.\n     tcAss!eT.  rtTec?eT.\n     rprTec!rCOO. Loop5OTAM\n\n  LHA = seekAcc!gDH.      \n          ((decAcc?rCOO. LHA)\n           +\n           (grntAcc?rCOO. takAct?gDH. LHA))\n\n  OTA = manVer?rCOO. chkAutAcc?oTAM. OTA\n\n  ET = tcAss?oTAM. rtTec!oTAM. ET\n\n  EV =  tecAss?oTAM. rptTec!oTAM. ET\n\n  HAS = perInvs?rCOO. rptInvs!rCOO. HAS\n\ninit\n  rC:RC || gDH:GDH || rCOO:RCOO || rHS:RHS ||\n  aC:AC || lHA:LHA || oTAM:OTAM || oTA:OTA ||\n  eT:ET || eV:EV   || hAS:HAS"
      -> "Healthcare example (realisable version), from Pal et al. in an ICTAC'25 publication using pomsets to analyse realisability. Our version uses synchronous communication, also described by Pal et al. When using instead asynchronous communication, an unbounded buffer is found.",
  )

  /** Description of the widgets that appear in the dashboard. */
  val widgets = List(
     "check well-formed" -> check(x => ateams.backend.TypeCheck.check(x).toSeq),
     "View pretty data" -> view[ASystem](Show.apply, Code("haskell")).moveTo(1),
//    "View structure" -> view(Show.mermaid, Mermaid),
//     "Well-formed?" -> view[ASystem](x => ateams.backend.TypeCheck.pp(x), Text).expand,
     "Well-behaved?" -> view[ASystem](x => ateams.backend.BehaviourCheck.randomWalk(St(x,Map()))._3.mkString("\n"), Text).expand,
     "Run semantics" -> steps(e=>St(e,Map()), Semantics, x=>Show/*.short*/(x), Show(_), Text).expand,
     "Build LTS" -> lts((e:ASystem)=>St(e,Map ()), Semantics, Show.showBuffers, Show(_)), //.moveTo(1),
     "Local components" ->
       viewMerms((sy: ASystem) =>
          for (nm,proc) <- sy.main.toList yield
              s"$nm:${Show(proc)}" -> SOS.toMermaid[Program.Act,Program.Proc](
                // SOS semantics
                new SOS[Program.Act,Program.Proc] {
                  override def next[A >: Program.Act](p: Program.Proc): Set[(A, Program.Proc)] =
                    Semantics.nextProc(p)(using St(sy,Map())).asInstanceOf[Set[(A,Program.Proc)]]
                }  ,
                proc, // initial state
                x=>"", // displaying states
                Show(_), // displaying labels
                80 // max size
              )).expand,
     "Number of states and edges"
      -> view((sy:ASystem) => {
          val (st,eds,done) = SOS.traverse(Semantics,St(sy,Map()),2000)
          //s"== Full state space ==" +
          (if !done then s"(Stopped after traversing 2000 states)"
           else s"States: ${st.size}\nEdges: $eds")
        },
        Text),

//     "Build LTS (explore)" -> ltsExplore(e=>e, Semantics, x=>Show(x.main), _.toString),
//    "Find strong bisimulation (given a program \"A ~ B\")" ->
//      compareStrongBisim(Semantics, Semantics,
//        (e: CCSSystem) => CCSSystem(e.defs, e.main, None),
//        (e: CCSSystem) => CCSSystem(e.defs, e.toCompare.getOrElse(Program.Term.End), None),
//        Show.justTerm, Show.justTerm, _.toString),
  )

  //// Documentation below

  override val footer: String =
    """Simple animator of A-Teams, using the
      | CAOS libraries. Source code available online:
      | <a target="_blank" href="https://github.com/FM-DCC/a-teams">
      | https://github.com/FM-DCC/a-team</a>.""".stripMargin


  override val documentation: Documentation = List(
    languageName -> "More information on the syntax of A-Team" ->
      ("A program in A-Team consists of <ul><li> [acts] a set of action declarations with their associated synchronisation types," +
      "</li><li> [proc] a set of process definitions, and</li><li> [init] a set of processes in parallel.</li></ul>" +
      "The grammar for processes is given by:\n" +
      "<pre>P := K |0 |α.P |P + P\nα:= a! |a? |a!n |a?n</pre>" +
      "\n Possible syncronisation types are \"sync\", \"fifo\", or \"unsorted\", and can also have a bound on the number" +
      "of receiving and sending agents, and a location type for the buffers (@snd, @rcv, @snd-rcv, or @global).")
  )
