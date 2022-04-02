package chiseltest.formal

import chisel3._

trait TSeqElement

case class AtmProp(signal:UInt) extends TSeqElement

case class TimeOp(lowerCycles: Int, upperCycles: Int) extends TSeqElement

case class Implication() extends TSeqElement

case class Leftbraket() extends TSeqElement

case class Rightbraket() extends TSeqElement

case class Not() extends TSeqElement
