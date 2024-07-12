// SPDX-License-Identifier: Apache-2.0
// modified in cha: to enable more model checking backends and algorithms
package chiseltest.formal

import chisel3.Module
import chiseltest.HasTestName
import chiseltest.formal.backends.FormalEngineAnnotation
import chiseltest.internal.TestEnvInterface
import chiseltest.simulator.{Compiler, WriteVcdAnnotation}
import firrtl.{AnnotationSeq, CircuitState}
import firrtl.annotations.NoTargetAnnotation
import firrtl.transforms.formal.DontAssertSubmoduleAssumptionsAnnotation
import sys.process._
import java.io._
import jhoafparser.parser.HOAFParser

sealed trait FormalOp extends NoTargetAnnotation
case class BoundedCheck(kMax: Int = -1) extends FormalOp
case class KInductionCheck(kMax: Int = -1) extends FormalOp
// IC3SA in pono can't generate witness, only work when properties can be proven... 
case class Ic3SaCheck(kMax: Int = -1) extends FormalOp

case object  EnSafetyOpti extends NoTargetAnnotation

/** Specifies how many cycles the circuit should be reset for. */
case class ResetOption(cycles: Int = 1) extends NoTargetAnnotation {
  require(cycles >= 0, "The number of cycles must not be negative!")
}

class FailedBoundedCheckException(val message: String, val failAt: Int) extends Exception(message)
private[chiseltest] object FailedBoundedCheckException {
  def apply(module: String, failAt: Int): FailedBoundedCheckException = {
    val msg = s"[$module] found an assertion violation $failAt steps after reset!"
    new FailedBoundedCheckException(msg, failAt)
  }
}

/** Adds the `verify` command for formal checks to a ChiselScalatestTester */
trait Formal { this: HasTestName =>
  def verify[T <: Module](dutGen: => T, annos: AnnotationSeq): Unit = {
    val withTargetDir = TestEnvInterface.addDefaultTargetDir(getTestName, annos)
    Formal.verify(dutGen, withTargetDir)
  }
}

/** An _escape hatch_ to disable more pessimistic modelling of undefined values. */
case object DoNotModelUndef extends NoTargetAnnotation

/** Disables firrtl optimizations when converting to a SMT/Btor2 system.
  * This is an escape hatch in case you suspect optimizations of doing something wrong!
  * Normally this annotation should *not* be needed!
  */
case object DoNotOptimizeFormal extends NoTargetAnnotation

private object Formal {
  def verify[T <: Module](dutGen: => T, annos: AnnotationSeq): Unit = {
    val ops = getOps(annos)
    assert(ops.nonEmpty, "No verification operation was specified!")
    val withDefaults = addDefaults(annos)

    // elaborate the design and compile to low firrtl
    val (highFirrtl, _) = Compiler.elaborate(() => dutGen, withDefaults)
    val lowFirrtl = Compiler.toLowFirrtl(highFirrtl, Seq(DontAssertSubmoduleAssumptionsAnnotation))

    // add reset assumptions
    val withReset = AddResetAssumptionPass.execute(lowFirrtl)
    
    val resetLength = AddResetAssumptionPass.getResetLength(withDefaults)
    ops.foreach(executeOp(withReset, resetLength, _))
  }

  val DefaultEngine: FormalEngineAnnotation = Z3EngineAnnotation
  def addDefaults(annos: AnnotationSeq): AnnotationSeq = {
    Seq(addDefaultEngine(_), addWriteVcd(_)).foldLeft(annos)((old, f) => f(old))
  }
  def addDefaultEngine(annos: AnnotationSeq): AnnotationSeq = {
    if (annos.exists(_.isInstanceOf[FormalEngineAnnotation])) { annos }
    else { DefaultEngine +: annos }
  }
  def addWriteVcd(annos: AnnotationSeq): AnnotationSeq = {
    if (annos.contains(WriteVcdAnnotation)) { annos }
    else { WriteVcdAnnotation +: annos }
  }
  def getOps(annos: AnnotationSeq): Seq[FormalOp] = {
    annos.collect { case a: FormalOp => a }.distinct
  }
  def executeOp(state: CircuitState, resetLength: Int, op: FormalOp): Unit = op match {
    // add kInduction engine, it only works when taking pono as the backend
    // since it only changes the command parameters of calling pono, bmc method is still used 
    case BoundedCheck(kMax) =>
      backends.Maltese.bmc(state.circuit, state.annotations, kMax = kMax, resetLength = resetLength)
    case KInductionCheck(kMax) => 
      backends.Maltese.bmc(state.circuit, state.annotations, kMax = kMax, resetLength = resetLength, KInductionCheck(kMax))
    case Ic3SaCheck(kMax) => 
      backends.Maltese.bmc(state.circuit, state.annotations, kMax = kMax, resetLength = resetLength, Ic3SaCheck(kMax))
  }
}
