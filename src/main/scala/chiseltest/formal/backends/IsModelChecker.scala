// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>
// modified in cha: more verification result types
package chiseltest.formal.backends

import firrtl.backends.experimental.smt._
import chiseltest.formal.{FormalOp, BoundedCheck}

private[chiseltest] trait ModelCheckResult {
  def isFail: Boolean
  def isSuccess: Boolean = !isFail
}
private[chiseltest] case class ModelCheckSuccess() extends ModelCheckResult { override def isFail: Boolean = false }
private[chiseltest] case class ModelCheckProve(pNum: Int) extends ModelCheckResult {override def isFail: Boolean = false }

private[chiseltest] case class ModelCheckFail(witness: Witness) extends ModelCheckResult {
  override def isFail: Boolean = true
}
private[chiseltest] case class ModelCheckFailNoWit() extends ModelCheckResult {
  override def isFail: Boolean = true
}

private[chiseltest] trait IsModelChecker {
  def name: String
  val prefix:        String
  val fileExtension: String
  def check(sys: TransitionSystem, kMax: Int = -1): ModelCheckResult
  // Scala disallows overloaded methods with default arguments
  def check(sys: TransitionSystem, kMax: Int, algor:FormalOp, nthProp:Int, checkCover: Boolean): ModelCheckResult = check(sys, kMax)
}

private[chiseltest] case class Witness(
  failed:  Seq[String],
  regInit: Map[Int, BigInt],
  memInit: Map[Int, Seq[(Option[BigInt], BigInt)]],
  inputs:  Seq[Map[Int, BigInt]])
