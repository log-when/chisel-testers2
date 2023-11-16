// SPDX-License-Identifier: Apache-2.0

package chiseltest.formal.backends.btor

import chiseltest.formal.backends._
import firrtl.backends.experimental.smt._
import chiseltest.formal.{FormalOp, BoundedCheck, KInductionCheck, Ic3SaCheck}

import scala.util.control.Breaks._
class BtormcModelChecker(targetDir: os.Path) extends IsModelChecker {
  override val fileExtension = ".btor2"
  override val name:   String = "btormc"
  override val prefix: String = "btormc"

  override def check(sys: TransitionSystem, kMax: Int): ModelCheckResult = {
    // serialize the system to btor2
    val filename = sys.name + ".btor"
    // btromc isn't happy if we include output nodes, so we skip them during serialization
    val lines = Btor2Serializer.serialize(sys, skipOutput = true)
    os.write.over(targetDir / filename, lines.mkString("", "\n", "\n"))

    // execute model checker
    val kmaxOpt = if (kMax > 0) Seq("--kmax", kMax.toString) else Seq()
    val cmd = Seq("btormc") ++ kmaxOpt ++ Seq(filename)
    val r = os.proc(cmd).call(cwd = targetDir, check = false)

    // write stdout to file for debugging
    val res = r.out.lines()
    os.write.over(targetDir / (filename + ".out"), res.mkString("", "\n", "\n"))

    // check to see if we were successful
    assert(r.exitCode == 0, s"We expect btormc to always return 0, not ${r.exitCode}. Maybe there was an error.")
    val isSat = res.nonEmpty && res.head.trim.startsWith("sat")
    
    if (isSat) {
      val witness = Btor2WitnessParser.read(res, 1).head
      ModelCheckFail(Btor2ModelChecker.convertWitness(sys, witness))
    } else {
      ModelCheckSuccess()
    }
  }
}

class PonoModelChecker(targetDir: os.Path) extends IsModelChecker
{
  override val fileExtension = ".btor2"
  override val name:   String = "pono"
  override val prefix: String = "pono"

  override def check(sys: TransitionSystem, kMax: Int): ModelCheckResult = {
    // serialize the system to btor2
    val filename = sys.name + ".btor"
    // btromc isn't happy if we include output nodes, so we skip them during serialization
    val lines = Btor2Serializer.serialize(sys, skipOutput = true)
    os.write.over(targetDir / filename, lines.mkString("", "\n", "\n"))

    // execute model checker
    val kmaxOpt = if (kMax > 0) Seq("-e","bmc","-k", kMax.toString, "--witness") else Seq()
    val cmd = Seq("pono") ++ kmaxOpt ++ Seq(filename)
    // println(s"cmd: $cmd")
    val r = os.proc(cmd).call(cwd = targetDir, check = false)

    // write stdout to file for debugging
    val res = r.out.lines()
    os.write.over(targetDir / (filename + ".out"), res.mkString("", "\n", "\n"))

    // check to see if we were successful
    // assert(r.exitCode == 0, s"We expect btormc to always return 0, not ${r.exitCode}. Maybe there was an error.")
    val isSat = res.nonEmpty && res.head.trim.startsWith("sat")

    val isSatNoWit = res.nonEmpty && res.head.trim.startsWith("IC3")
    // println(s"isSatNoWit: $isSatNoWit")
    if (isSat) {
      val witness = Btor2WitnessParser.read(res, 1).head
      ModelCheckFail(Btor2ModelChecker.convertWitness(sys, witness))
    } else if(isSatNoWit){
      ModelCheckFailNoWit()
    } else {
      ModelCheckSuccess()
    }
  }

  override def check(sys: TransitionSystem, kMax: Int, algor: FormalOp): ModelCheckResult = {
    val filename = sys.name + ".btor"
    // btromc isn't happy if we include output nodes, so we skip them during serialization
    val lines = Btor2Serializer.serialize(sys, skipOutput = true)
    os.write.over(targetDir / filename, lines.mkString("", "\n", "\n"))

    // execute model checker
    val badNum = sys.signals.count(_.lbl == IsBad)
    val badSeq = Seq.range(0, badNum ,1)
    var result: ModelCheckResult = ModelCheckSuccess()
    breakable
    {
      badSeq.foreach{ 
        badNu:Int =>
        {
          result = PonoModelChecker.checkProperty(targetDir, filename,sys, kMax, algor, badNu)
          if(result.isInstanceOf[ModelCheckFail])
            break()
        }
      }
    }
    result
  }
}

object PonoModelChecker
{
  def engineCommand(algor:FormalOp) = {
    algor match{
      case x: BoundedCheck => "bmc"
      case x: KInductionCheck => "ind"
      case x: Ic3SaCheck => "ic3sa"
      case _ => "bmc"
    }
  }

  def checkProperty(targetDir: os.Path, filename:String, sys: TransitionSystem, kMax: Int, algor: FormalOp, badNum: Int):ModelCheckResult ={
    val kmaxOpt = if (kMax > 0) Seq("-e",PonoModelChecker.engineCommand(algor), 
    "-p", badNum.toString, "-k", kMax.toString, "--witness") else Seq()
      // Seq("-e", PonoModelChecker.engineCommand(algor)) ++
      // Seq("-p", badNum.toString) ++ 
      // Seq("-k", kMax.toString, "--witness") else Seq()
    val cmd = Seq("pono") ++ kmaxOpt ++ Seq(filename)
    
    // println(s"cmd: $cmd")
    val r = os.proc(cmd).call(cwd = targetDir, check = false)

    // write stdout to file for debugging
    val res = r.out.lines()
    os.write.over(targetDir / (filename + ".out"), res.mkString("", "\n", "\n"))

    // check to see if we were successful
    // assert(r.exitCode == 0, s"We expect btormc to always return 0, not ${r.exitCode}. Maybe there was an error.")
    val isSat = res.nonEmpty && res.head.trim.startsWith("sat")
    val isUnSat = res.nonEmpty && res.head.trim.startsWith("unsat")
    val isSatNoWit = res.nonEmpty && res.head.trim.startsWith("IC3")
    // println(s"isSatNoWit: $isSatNoWit")

    if (isSat) {
      val witness = Btor2WitnessParser.read(res, 1).head
      ModelCheckFail(Btor2ModelChecker.convertWitness(sys, witness))
    } else if (isUnSat) {
      ModelCheckProve(badNum)
    } else if(isSatNoWit){
      ModelCheckFailNoWit()
    } else {
      ModelCheckSuccess()
    }
  }
}

object Btor2ModelChecker {
  def convertWitness(sys: TransitionSystem, bw: Btor2Witness): Witness = {
    val badNames = sys.signals.filter(_.lbl == IsBad).map(_.name).toIndexedSeq
    val failed = bw.failed.map(badNames)
    Witness(failed, bw.regInit, bw.memInit, bw.inputs)
  }
}
