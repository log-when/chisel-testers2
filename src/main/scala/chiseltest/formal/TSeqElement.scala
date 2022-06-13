// package chiseltest.formal

// //import scala.language.implicitConversions
// import chisel3._


// trait svaElement extends TSeqElementAnno

// case class AtmProp(signal:Bool) extends svaElement
// //case class resetAnno(target:Target) extends TSeqElementAnno

// trait s_seqElement extends svaElement
// trait s_propElement extends svaElement

// case class Leftbraket() extends svaElement
// case class Rightbraket() extends svaElement


// case class TimeOp(lowerCycles: Int, upperCycles: Int) extends s_seqElement
// case class RepetOp(lowerBounds: Int, upperBounds: Int) extends svaElement

// case class Implication() extends s_propElement
// case class NotOp() extends s_propElement
// case class FinallOp() extends s_propElement
// case class GlobalOp() extends s_propElement
// case class NextOp() extends s_propElement







// object TTSeq
// {
//     implicit def uint2Atm(signal:Bool): TTSeq = new TTSeq(Seq(AtmProp(signal)))    
//     //implicit def TSeqEle2Seq(te: svaElement): TTSeq = new TTSeq(Seq(te))
//     def ap(signal:Bool): TTSeq = new TTSeq(Seq(AtmProp(signal)))
//     def ###(lowerCycles: Int, upperCycles: Int): TTSeq = new TTSeq(Seq(TimeOp(lowerCycles, upperCycles)))
//     def G: TTSeq = new TTSeq(Seq(GlobalOp()))
//     def F: TTSeq = new TTSeq(Seq(FinallOp()))
//     def printlnTSeq(t: TTSeq) = {println(t.s)}
// }

// class TTSeq(val s: Seq[svaElement])
// {
//     def G(t:TTSeq): TTSeq = new TTSeq((s :+ GlobalOp()) ++ t.s)
//     def F(t:TTSeq): TTSeq = new TTSeq((s :+ FinallOp()) ++ t.s)

//     def ap(t:TTSeq): TTSeq = 
//     {
        
//         //val tte = AtmProp(signal)
//         //val ttee = Seq(tte)
//         //println(s"left: $s")
//         //println(s"right: ${t.s}")
//         new TTSeq(s ++ t.s)
//     }
//     // implicit def uint2Atm(signal:Bool): TTSeq = new TTSeq(Seq(AtmProp(signal)))
//     def ###(lowerCycles: Int, upperCycles: Int): TTSeq = new TTSeq((s :+ TimeOp(lowerCycles, upperCycles)))
//     def |->(t:TTSeq) = new TTSeq((s :+ Implication()) ++ t.s)
//     /*def |->() = new TTSeq(s :+ Implication())*/
//     //def |-> = new TTSeq(s :+ Implication()) 
// }

// /*class TNode(op: svaElement, left: TNode, right: TNode, isProperty:Boolean)
// {
//     def ap(tn: TNode): TNode = 
//     {
//         new TNode(AtmProp())
//     }
// }*/