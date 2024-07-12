// SPDX-License-Identifier: Apache-2.0
// modified in cha: handle the aux vars added in L2S
package chiseltest.formal.backends

import chiseltest.formal._
import chiseltest.formal.backends.btor.{BtormcModelChecker, PonoModelChecker}
import chiseltest.formal.backends.smt._
import chiseltest.formal.{DoNotModelUndef, DoNotOptimizeFormal, FailedBoundedCheckException}
import firrtl._
import firrtl.annotations._
import firrtl.stage._
import firrtl.backends.experimental.smt.random._
import firrtl.backends.experimental.smt._
import chiseltest.simulator._
import firrtl.options.Dependency

sealed trait FormalEngineAnnotation extends NoTargetAnnotation

/** Use a SMTLib based model checker with the CVC4 SMT solver.
  * @note CVC4 often performs better than Z3.
  */
case object CVC4EngineAnnotation extends FormalEngineAnnotation

/** Use a SMTLib based model checker with the Z3 SMT solver.
  * @note Z3 is the most widely available and easiest to install SMT solver.
  */
case object Z3EngineAnnotation extends FormalEngineAnnotation

/** Uses the btormc model checker from the boolector code base.
  * @note btormc is generally faster than Z3 or CVC4 but needs to be built from source
  */
case object BtormcEngineAnnotation extends FormalEngineAnnotation

/** Uses the pono model checker, similar to btormc 
  * @note pono performs well in HWMCC 20, 
  * @note it is generally faster than btormc but also needs to be built from source
  * @note pono has implemented more model checking algorithm but can only check one assertion every call 
  */
case object PonoEngineAnnotation extends FormalEngineAnnotation

/** Use a SMTLib based model checker with the yices2 SMT solver.
  * @note yices2 often performs better than Z3 or CVC4.
  * @note yices2 is not supported yet, because we have not figured out yet how to deal with memory initialization
  */
private case object Yices2EngineAnnotation extends FormalEngineAnnotation

/** Use a SMTLib based model checker with the boolector SMT solver.
  * @note boolector often performs better than Z3 or CVC4.
  * @note boolecter is not supported, because some bugs that were fixed in bitwuzla still remain in boolector
  *       leading to crashes when trying to get-value of arrays.
  */
private case object BoolectorEngineAnnotation extends FormalEngineAnnotation

/** Use a SMTLib based model checker with the bitwuzla SMT solver.
  * @note bitwuzla often performs better than Z3 or CVC4.
  */
case object BitwuzlaEngineAnnotation extends FormalEngineAnnotation

/** Formal Verification based on the firrtl compiler's SMT backend and the maltese SMT libraries solver bindings. */
private[chiseltest] object Maltese {
  def bmc(circuit: ir.Circuit, annos: AnnotationSeq, kMax: Int, resetLength: Int = 0, algor: FormalOp = BoundedCheck()): Unit = {
    require(kMax > 0)
    require(resetLength >= 0)

    // convert to transition system
    val targetDir = Compiler.requireTargetDir(annos)
    val modelUndef = !annos.contains(DoNotModelUndef)
    val sysAnnos: AnnotationSeq = if (modelUndef) { DefRandomAnnos ++: annos }
    else { annos }
    val sysInfo = toTransitionSystem(circuit, sysAnnos)

    // if the system has no bad states => success!
    if (noBadStates(sysInfo.sys)) {
      return // proven correct by the compiler!
    }

    // perform check
    val checkers = makeCheckers(annos, targetDir)
    assert(checkers.size == 1, "Parallel checking not supported atm!")

    // currently, only pono can call k-Induction algorithm to prove the correctness of design
    val proveChecker = checkers.head.isInstanceOf[PonoModelChecker] 
    val proveAlgor = algor match { 
      case _: KInductionCheck => true
      case _: Ic3SaCheck => true
      case _ => false
    }
    assert(!proveAlgor | proveChecker, s"${checkers.head.name} can't prove property!")

    checkers.head.check(sysInfo.sys, kMax = kMax + resetLength, algor) match {
      case ModelCheckFail(witness) =>
        val writeVcd = annos.contains(WriteVcdAnnotation)
        if (writeVcd) {
          val hasCHA = !noCHA(sysInfo.sys)
          // println(s"hasCHA: ${hasCHA}")
          // if there is no cha, the simulation is from original one
          if(!hasCHA)
          {
            val sim = new TransitionSystemSimulator(sysInfo.sys)
            sim.run(witness, vcdFileName = Some((targetDir / s"${circuit.main}.bmc.vcd").toString))
            val trace = witnessToTrace(sysInfo, witness)
            val treadleState = prepTreadle(circuit, annos, modelUndef)
            val treadleDut = TreadleBackendAnnotation.getSimulator.createContext(treadleState)
            Trace.replayOnSim(trace, treadleDut)
          }
          // if the violated safety property is from extended SVA-like assertion
          // don't simulate, only output the lasso-shaped trace 
          else
          {
            val inputNameMap = sysInfo.sys.inputs.map(_.name).map(name => name -> sysInfo.stateMap.getOrElse(name, name)).toMap
            // println(s"inputNameMap: ${inputNameMap}")
            //- show aux state temporarily
            val stateNameMap = sysInfo.sys.states.map(_.name).map(name => name -> {
                sysInfo.stateMap.getOrElse(name, name)
            }).toMap 

            // println(s"states: ${sysInfo.sys.states}")
            // println(s"stateNameMap: ${stateNameMap}")
            // to be optimized
            // if triggered property is not liveness property, aux state variable can be hidden
            val badString = witness.failed(0)
            val triggerJustice = triggerJust(badString)
            // println(s"triggerJustice: ${triggerJustice}")

            val sim = new TransitionSystemSimulator(sysInfo.sys, inputNameMap, stateNameMap, triggerJustice)
            sim.run(witness, vcdFileName = Some((targetDir / s"${circuit.main}.bmc.vcd").toString))
            val trace = witnessToTrace(sysInfo, witness)
            val treadleState = prepTreadle(circuit, annos, modelUndef)
            val treadleDut = TreadleBackendAnnotation.getSimulator.createContext(treadleState)
            Trace.replayOnSim(trace, treadleDut)
          }
        }
        val failSteps = witness.inputs.length - 1 - resetLength
        throw FailedBoundedCheckException(circuit.main, failSteps)
      
      case ModelCheckFailNoWit() => throw FailedBoundedCheckException(circuit.main, -1)
      case ModelCheckSuccess() => // good!
      case ModelCheckProve(badNum) => // good!
    }
  }

  // compile low firrtl circuit into a version with all DefRandom registers so that treadle can use it to replay the
  // counter example
  private def prepTreadle(circuit: ir.Circuit, annos: AnnotationSeq, modelUndef: Boolean): CircuitState = {
    if (!modelUndef) { CircuitState(circuit, annos) }
    else {
      val res = firrtlPhase.transform(
        Seq(
          RunFirrtlTransformAnnotation(new LowFirrtlEmitter),
          new CurrentFirrtlStateAnnotation(Forms.LowForm),
          FirrtlCircuitAnnotation(circuit)
        ) ++: annos ++: DefRandomTreadleAnnos
      )
      Compiler.annosToState(res)
    }
  }

  private val LoweringAnnos: AnnotationSeq = Seq(
    // we need to flatten the whole circuit
    RunFirrtlTransformAnnotation(Dependency(FlattenPass)),
    RunFirrtlTransformAnnotation(Dependency[firrtl.passes.InlineInstances])
  )

  private val Optimizations: AnnotationSeq = Seq(
    RunFirrtlTransformAnnotation(Dependency[firrtl.transforms.ConstantPropagation]),
    RunFirrtlTransformAnnotation(Dependency(passes.CommonSubexpressionElimination)),
    RunFirrtlTransformAnnotation(Dependency[firrtl.transforms.DeadCodeElimination])
  )

  private val DefRandomAnnos: AnnotationSeq = Seq(
    RunFirrtlTransformAnnotation(Dependency(UndefinedMemoryBehaviorPass)),
    RunFirrtlTransformAnnotation(Dependency(InvalidToRandomPass))
  )

  private val DefRandomTreadleAnnos: AnnotationSeq =
    RunFirrtlTransformAnnotation(Dependency(DefRandToRegisterPass)) +: DefRandomAnnos

  private case class SysInfo(sys: TransitionSystem, stateMap: Map[String, String], memDepths: Map[String, Int])

  private def toTransitionSystem(circuit: ir.Circuit, annos: AnnotationSeq): SysInfo = {

    val logLevel = Seq() // Seq("-ll", "info")
    val opts: AnnotationSeq = if (annos.contains(DoNotOptimizeFormal)) Seq() else Optimizations
    //SMTEmitter depends on FirrtlToTransitionSystem transform 
    val res = firrtlPhase.transform(
      Seq(
        RunFirrtlTransformAnnotation(Dependency(SMTLibEmitter)),
        new CurrentFirrtlStateAnnotation(Forms.LowForm),
        FirrtlCircuitAnnotation(circuit)
      ) ++: logLevel ++: annos ++: LoweringAnnos ++: opts
    )
    val stateMap = FlattenPass.getStateMap(circuit.main, res)
    // println(s"stateMap: ${stateMap}")
    val memDepths = FlattenPass.getMemoryDepths(circuit.main, res)
    val sys = res.collectFirst { case TransitionSystemAnnotation(s) => s }.get
    
    // print the system, convenient for debugging, might disable once we have done more testing
    if (true) {
      val targetDir = Compiler.requireTargetDir(annos)
      os.write.over(targetDir / s"${circuit.main}.sys", sys.serialize)
    }

    SysInfo(sys, stateMap, memDepths)
  }

  private def noBadStates(sys: TransitionSystem): Boolean =
    sys.signals.count(_.lbl == IsBad) == 0

  private def noCHA(sys: TransitionSystem): Boolean =
    sys.states.count(_.name.slice(0,9) == "assertSta") == 0 && sys.states.count(_.name.slice(0,9) == "assumeSta") == 0

  private def triggerJust(badString: String): Boolean =
    badString.slice(0,8) == "just2Bad"

  private def firrtlPhase = new FirrtlPhase

  private def makeCheckers(annos: AnnotationSeq, targetDir: os.Path): Seq[IsModelChecker] = {
    val engines = annos.collect { case a: FormalEngineAnnotation => a }
    assert(engines.nonEmpty, "You need to provide at least one formal engine annotation!")
    engines.map {
      case CVC4EngineAnnotation      => new SMTModelChecker(CVC4SMTLib)
      case Z3EngineAnnotation        => new SMTModelChecker(Z3SMTLib)
      case BtormcEngineAnnotation    => new BtormcModelChecker(targetDir)
      case PonoEngineAnnotation => new PonoModelChecker(targetDir)
      case Yices2EngineAnnotation    => new SMTModelChecker(Yices2SMTLib)
      case BoolectorEngineAnnotation => new SMTModelChecker(BoolectorSMTLib)
      case BitwuzlaEngineAnnotation  => new SMTModelChecker(BitwuzlaSMTLib)
    }
  }

  private def expandMemWrites(depth: Int, values: Seq[(Option[BigInt], BigInt)]): Seq[BigInt] = {
    var mem = Vector.fill(depth)(BigInt(0))
    values.foreach {
      case (None, value) =>
        mem = Vector.fill(depth)(value)
      case (Some(addr), value) =>
        // ignore out of bounds results (on the SMT level, the memory depth is always a power of two)
        if (addr < depth) { mem = mem.updated(addr.toInt, value) }
    }
    mem
  }

  private def witnessToTrace(sysInfo: SysInfo, w: Witness): Trace = {
    val inputNames = sysInfo.sys.inputs
      .map(_.name)
      // DefRand nodes are modelled as inputs for the formal engine,
      // but will be turned into registers for the replay on treadle
      // thus we need to translate them to a non-flattened path
      .map(name => sysInfo.stateMap.getOrElse(name, name))
      .toIndexedSeq
    val stateNames = sysInfo.sys.states
      .map(_.name)
      .map(name => sysInfo.stateMap.getOrElse(name, name))
      .toIndexedSeq

    Trace(
      inputs = w.inputs.map(_.toSeq.filter{case (i,_) => sysInfo.sys.inputs.contains(i)}.map { case (i, value) => inputNames(i) -> value }),
      regInit = w.regInit.toSeq.filter{case (i,_) => sysInfo.sys.states.contains(i)}.map { case (i, value) => stateNames(i) -> value },
      memInit = w.memInit.toSeq.filter{case (i,_) => sysInfo.sys.states.contains(i)}.map { case (i, values) =>
        val name = stateNames(i)
        name -> expandMemWrites(sysInfo.memDepths(name), values)
      }
    )
  }

}
