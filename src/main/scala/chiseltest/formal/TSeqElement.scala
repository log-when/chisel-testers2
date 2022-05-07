package chiseltest.formal

//import scala.language.implicitConversions
import chisel3._

trait TSeqElement

case class AtmProp(signal:Bool) extends TSeqElement

case class TimeOp(lowerCycles: Int, upperCycles: Int) extends TSeqElement

case class Implication() extends TSeqElement

case class Leftbraket() extends TSeqElement

case class Rightbraket() extends TSeqElement

case class NotOp() extends TSeqElement

case class FinallOp() extends TSeqElement

case class GlobalOp() extends TSeqElement

case class NextOp() extends TSeqElement

case class RepetOp(lowerBounds:Int, upperBounds: Int) extends TSeqElement


object TTSeq
{
    implicit def uint2Atm(signal:Bool): TTSeq = new TTSeq(Seq(AtmProp(signal)))    
    //implicit def TSeqEle2Seq(te: TSeqElement): TTSeq = new TTSeq(Seq(te))
    def ap(signal:Bool): TTSeq = new TTSeq(Seq(AtmProp(signal)))
    def ###(lowerCycles: Int, upperCycles: Int): TTSeq = new TTSeq(Seq(TimeOp(lowerCycles, upperCycles)))
    def F(t: TTSeq): TTSeq = new TTSeq(Seq(FinallOp()) ++ t.s)
    def printlnTSeq(t: TTSeq) = {println(t.s)}
}

class TTSeq(val s: Seq[TSeqElement])
{
    /*def apply(t:TTSeq): TTSeq = 
    {
        //println(s"left: $s")
        //println(s"right: ${t.s}")
        new TTSeq(s ++ t.s)
    }*/ 
    def ap(t:TTSeq): TTSeq = 
    {
        
        //val tte = AtmProp(signal)
        //val ttee = Seq(tte)
        println(s"left: $s")
        println(s"right: ${t.s}")
        new TTSeq(s ++ t.s)
    }
    // implicit def uint2Atm(signal:Bool): TTSeq = new TTSeq(Seq(AtmProp(signal)))
    def ###(lowerCycles: Int, upperCycles: Int): TTSeq = new TTSeq((s :+ TimeOp(lowerCycles, upperCycles)))
    def |->(t:TTSeq) = new TTSeq((s :+ Implication()) ++ t.s)
    /*def |->() = new TTSeq(s :+ Implication())*/
    //def |-> = new TTSeq(s :+ Implication()) 
}

/*class TNode(op: TSeqElement, left: TNode, right: TNode, isProperty:Boolean)
{
    def ap(tn: TNode): TNode = 
    {
        new TNode(AtmProp())
    }
}*/