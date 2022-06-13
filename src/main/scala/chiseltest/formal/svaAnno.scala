package chiseltest.formal

import scala.collection.mutable
import chisel3._
import chisel3.experimental.{ChiselAnnotation,annotate,RunFirrtlTransform}
import firrtl.annotations.{Annotation, ReferenceTarget, SingleTargetAnnotation, Target,MultiTargetAnnotation}
import firrtl.options.StageUtils
import firrtl.RenameMap
import org.json4s.JValue
import firrtl._

import scala.collection.Traversable


trait svaElement
trait svaElementAnno
trait s_seqOp extends svaElement
trait s_propOp extends svaElement
trait s_seqUnOp extends s_seqOp
trait s_seqBinOp extends s_seqOp
trait s_propUnOp extends s_propOp
trait s_propBinOp extends s_propOp

case class Semicolon() extends svaElementAnno
case class LeftBrace() extends svaElementAnno
case class RightBrace() extends svaElementAnno

case class ResetAnno(target:Target) extends svaElementAnno
case class Leftbraket() extends svaElement with svaElementAnno
case class Rightbraket() extends svaElement with svaElementAnno
case class ImplicationOp() extends svaElement with svaElementAnno

case class AtmProp(signal:Bool) extends svaElement
case class AtmPropAnno(target:Target) extends svaElementAnno 
case class TimeOp(lowerBounds: Int, upperBounds: Int) extends s_seqBinOp with svaElementAnno
case class RepetOp(lowerBounds: Int, upperBounds: Int) extends s_seqUnOp with svaElementAnno
case class OrSeqOp() extends s_seqBinOp with svaElementAnno


case class NotOp() extends s_propUnOp with svaElementAnno
case class FinallOp() extends s_propUnOp with svaElementAnno
case class GlobalOp() extends s_propUnOp with svaElementAnno
case class NextOp() extends s_propUnOp with svaElementAnno
case class UntilOp() extends s_propBinOp with svaElementAnno
case class AndOp() extends s_propBinOp with svaElementAnno
case class OrOp() extends s_propBinOp with svaElementAnno


object svaSeq
{
    // def apply(t:svaSeq): svaSeq = 
    // {
    //   println(s"t: $t")
    //   new svaSeq((Seq(Leftbraket()) ++ t.s) :+ Rightbraket())
    // }
    def |- : svaSeq = new svaSeq(Seq(Leftbraket()))
    //def -| : svaSeq = new svaSeq(Seq(Rightbraket()))
    implicit def uint2Atm(signal:Bool): svaSeq = new svaSeq(Seq(AtmProp(signal)))    
    def ap(signal:Bool): svaSeq = new svaSeq(Seq(AtmProp(signal)))
    def ###(lowerBounds: Int, upperBounds: Int): svaSeq = new svaSeq(Seq(TimeOp(lowerBounds, upperBounds)))
    def |(t:svaSeq): svaSeq = new svaSeq(Seq(OrSeqOp()) ++ t.s)
    def | : svaSeq = new svaSeq(Seq(OrSeqOp()))
    def *(lowerBounds: Int, upperBounds: Int): svaSeq = new svaSeq(Seq(RepetOp(lowerBounds, upperBounds)))
    def G: svaSeq = new svaSeq(Seq(GlobalOp()))
    def F: svaSeq = new svaSeq(Seq(FinallOp()))
    def X: svaSeq = new svaSeq(Seq(NextOp()))
    def U: svaSeq = new svaSeq(Seq(UntilOp()))
    def unary_! : svaSeq = new svaSeq(Seq(NotOp()))
    // def && : svaSeq = new svaSeq((AndOp()) ++ t.s)
    def || : svaSeq = new svaSeq(Seq(OrOp()))
    // def printlnTSeq(t: svaSeq) = {println(t.s)}
}

class svaSeq(val s: Seq[svaElement])
{    
    // def apply(t:svaSeq): svaSeq = 
    // {
    //   println(s"t.s: ${t.s}")
    //   new svaSeq(((s :+ Leftbraket()) ++ t.s) :+ Rightbraket())
    // }  
    def ap(t:svaSeq): svaSeq = new svaSeq(s ++ t.s)
    
    def |- (t:svaSeq): svaSeq = new svaSeq((s :+ Leftbraket()) ++ t.s)
    def -| : svaSeq = new svaSeq(s :+ Rightbraket())
    def -|(t:svaSeq): svaSeq = new svaSeq((s :+ Rightbraket()) ++ t.s)

    def |(t:svaSeq): svaSeq = new svaSeq((s :+ OrSeqOp()) ++ t.s)
    def ###(lowerBounds: Int, upperBounds: Int): svaSeq = new svaSeq((s :+ TimeOp(lowerBounds, upperBounds)))
    def *(lowerBounds: Int, upperBounds: Int): svaSeq = new svaSeq((s :+ RepetOp(lowerBounds, upperBounds)))
    def |->(t:svaSeq) = new svaSeq((s :+ ImplicationOp()) ++ t.s)
   
    def G : svaSeq = new svaSeq(s :+ GlobalOp())

    def G(t:svaSeq): svaSeq = new svaSeq((s :+ GlobalOp()) ++ t.s)
    def F(t:svaSeq): svaSeq = new svaSeq((s :+ FinallOp()) ++ t.s)
    def U(t:svaSeq): svaSeq = new svaSeq((s :+ UntilOp()) ++ t.s)
    def X(t:svaSeq): svaSeq = new svaSeq((s :+ NextOp()) ++ t.s)

    def unary_! : svaSeq = new svaSeq((NotOp() +: s))
    def !(t:svaSeq): svaSeq = new svaSeq((s :+ NotOp()) ++ t.s)
    
    def && : svaSeq = new svaSeq(s :+ AndOp())
    def || : svaSeq = new svaSeq(s :+ OrOp())
    def &&(t:svaSeq): svaSeq = new svaSeq((s :+ AndOp()) ++ t.s)
    def ||(t:svaSeq): svaSeq = new svaSeq((s :+ OrOp()) ++ t.s)
    
}

case class SVANode(ele:svaElementAnno, left:SVANode, right: SVANode, isProperty: Boolean = false)
object svaSeqAnno
{
  def makeSVAAnno(currReset:Reset, sva:svaSeq) = {
    annotate(new ChiselAnnotation {
      // Conversion to FIRRTL Annotation 
      override def toFirrtl: Annotation = 
      {
        println(s"currReset: ${sva.s}")
        val svaanotation : Seq[Seq[svaElementAnno]] = sva.s map {
          case AtmProp(ap) => Seq(AtmPropAnno(ap.toTarget))
          case otherOp: svaElementAnno => Seq(otherOp)
        } 
        new svaSeqAnno(svaanotation:+Seq(ResetAnno(currReset.toTarget)))
      }
    })
  }

  def inSeq2PSL(seq:Seq[svaElementAnno], rename2p: Map[Target,String]) : String = 
  {
    var psl =""
    for(i <- 0 until seq.size)
    {
      val temp:String = {
        seq(i) match {
          case AtmPropAnno(target) => rename2p(target) 
          case bsp:s_seqBinOp =>
          {
            bsp match {
              case TimeOp(lb,ub) =>
              {
                if(i == seq.size - 1 || seq(i+1).isInstanceOf[Rightbraket] || seq(i).isInstanceOf[s_propBinOp] || seq(i).isInstanceOf[s_propUnOp])
                {
                  if(i == 0 || seq(i).isInstanceOf[Leftbraket] || seq(i).isInstanceOf[TimeOp])
                    if(ub != -1)
                      "true[*" + (lb-1) + ".." + ub + "]"
                    else 
                      "true[*" + (lb-1) + "]" + ";" + "true[*]"
                  else
                    if(ub != -1)
                      ";true[*" + (lb-1) + ".." + ub + "]"
                    else 
                      ";true[*" + (lb-1) + "]" + ";" + "true[*]"
                }
                else
                {
                  val upperBounds:String = if(ub == -1) "$" else ub.toString()
                  " ##[" + lb + ":" + upperBounds + "]"
                }              
              }
              case OrSeqOp() => " | "
            }
          }
          case bup:s_seqUnOp =>
          {
            bup match {
              case RepetOp(lb,ub) =>
              {
                val upperBounds:String = if(ub == -1) "$" else ub.toString()
                " [*" + lb + ":" + ub + "]"
              }
            }
          }
          case Leftbraket() => "("
          case Rightbraket() => ")"
        }
      }
      psl += temp
    }
    psl
  }
  def noParetoPSL(seq:Seq[svaElementAnno], rename2p: Map[Target,String]) : String = 
  {
    var psl:String = ""
    var inSeq = false
    for(i <- 0 until seq.size)
    {
      if(!inSeq)
      {
        println(s"i: $i")
        println(seq(i).toString())
        if(seq(i).isInstanceOf[AtmPropAnno] || seq(i).isInstanceOf[TimeOp] )
        {
          inSeq = true
          psl += "{"
        }
      }
      else
      {
        println(s"i_inSeq: $i")
        println(seq(i).toString())
        if(seq(i).isInstanceOf[ImplicationOp] || seq(i).isInstanceOf[s_propBinOp] || seq(i).isInstanceOf[s_propUnOp])
        {
          psl += "}"
          inSeq = false
        }
      }
      var temp:String = {
        seq(i) match {
        case AtmPropAnno(target) => rename2p(target)
        case up:s_propUnOp => 
        {
          up match {
            case NotOp() => "! "
            case FinallOp() => "F "
            case GlobalOp() => "G "
            case NextOp() => "X "
            case _ => throw new ClassCastException("Unsupported union property operator")
          }
        }
        case bp:s_propBinOp =>
        {
          bp match {
            case AndOp() => " & "
            case OrOp() => " | "
            case UntilOp() => " U "
            case _ => throw new ClassCastException("Unsupported union property operator")
          }
        }
        case bsp:s_seqBinOp =>
        {
          bsp match {
            case TimeOp(lb,ub) =>
            {
              if(i == seq.size - 1 || seq(i+1).isInstanceOf[ImplicationOp] || seq(i+1).isInstanceOf[s_propUnOp])
              {
                if(i == 0 || seq(i-1).isInstanceOf[TimeOp])
                  if(ub != -1)
                    "true[*" + (lb-1) + ".." + ub + "]"
                  else 
                    "true[*" + (lb-1) + "]" + ";" + "true[*]"
                else
                  if(ub != -1)
                    ";true[*" + (lb-1) + ".." + ub + "]"
                  else 
                    ";true[*" + (lb-1) + "]" + ";" + "true[*]"
              }
              else
              {
                val upperBounds:String = if(ub == -1) "$" else ub.toString()
                " ##[" + lb + ":" + upperBounds + "]"
              }
            }
            case OrSeqOp() => " | "
          }
        }
        case bup:s_seqUnOp =>
        {
          bup match {
            case RepetOp(lb,ub) =>
            {
              val upperBounds:String = if(ub == -1) "$" else ub.toString()
              " [*" + lb + ":" + upperBounds + "]"
            }
          }
        }
        case ImplicationOp() => "[]->"
        }
      }
      psl += temp
      if(i==seq.size-1 && inSeq )
      {
        psl += "}!"
        inSeq = false
      }
    }
    psl
  }

  def toPSL(seq:Seq[svaElementAnno], rename2p: Map[Target,String], inSeq_ :Boolean) : String = 
  {
    if(inSeq_)
      inSeq2PSL(seq, rename2p)
    else
    {
      val firstLB = seq.indexWhere(_.isInstanceOf[Leftbraket])
      if(firstLB == -1)
      {
        noParetoPSL(seq,rename2p)
      }
      else
      {
        var n = 1
        var corRB = -1
        var ind = firstLB + 1
        while( n!=0 && ind < seq.size)
        {
          var n = 1
          if(seq(ind).isInstanceOf[Leftbraket])
            n += 1
          else if(seq(ind).isInstanceOf[Rightbraket])
            n -= 1
          if(n==0)
            corRB = ind
          ind += 1
        }
        val findPropOp = seq.slice(firstLB,corRB+1).indexWhere
        {
          case p:svaElementAnno => p.isInstanceOf[s_propOp] || p.isInstanceOf[ImplicationOp] 
        }
        if(findPropOp == -1)
        {
          val lastProOp = seq.slice(0,firstLB).lastIndexWhere
          {
            case p:svaElementAnno => p.isInstanceOf[s_propOp] || p.isInstanceOf[ImplicationOp] 
          }
          val extendLB = lastProOp + 1
          val nextProOp = seq.slice(corRB+1,seq.size).indexWhere
          {
            case p:svaElementAnno => p.isInstanceOf[s_propOp] || p.isInstanceOf[ImplicationOp] 
          }
          val extendRB = if(nextProOp == -1) seq.size-1 else corRB + nextProOp

          println(s"left subseq: ${seq.slice(0,extendLB)}")
          println(s"middle subseq: ${seq.slice(extendLB,extendRB+1)}")
          println(s"right subseq: ${seq.slice(extendRB+1,seq.size)}")

          if(nextProOp == -1)
            noParetoPSL(seq.slice(0,extendLB),rename2p) + "{" + inSeq2PSL(seq.slice(extendLB,extendRB+1),rename2p) + "}!" + toPSL(seq.slice(extendRB+1,seq.size),rename2p,false)
          else
            noParetoPSL(seq.slice(0,extendLB),rename2p) + "{" + inSeq2PSL(seq.slice(extendLB,extendRB+1),rename2p) + "}" + toPSL(seq.slice(extendRB+1,seq.size),rename2p,false)
        }
        else
        {
          noParetoPSL(seq.slice(0,firstLB),rename2p) + "(" + toPSL(seq.slice(firstLB+1,corRB),rename2p,false) + ")" + toPSL(seq.slice(corRB+1,seq.size),rename2p,false)
        }
      }
    }
    
  }

  // @throws(classOf[RuntimeException])
  // def toPSL(seq:Seq[svaElementAnno], rename2p: Map[Target,String], inSeq_ :Boolean, needEndBrace :Boolean) : String = 
  // {
  //   var psl:String = ""
  //   var inSeq:Boolean = inSeq_
  //   val lastSeqEle = seq.lastIndexWhere{case p:svaElementAnno => p.isInstanceOf[AtmPropAnno] || p.isInstanceOf[s_seqOp]}
  //   var i = 0
  //   while(i < seq.size)
  //   {
  //     if(!inSeq)
  //     {
  //       println(s"i: $i")
  //       println(seq(i).toString())
  //       if(seq(i).isInstanceOf[AtmPropAnno] || seq(i).isInstanceOf[s_seqOp] )
  //       {
  //         inSeq = true
  //         psl += "{"
  //       }
  //     }
  //     else
  //     {
  //       println(s"i_inSeq: $i")
  //       println(seq(i).toString())
  //       // if(seq(i).isInstanceOf[AtmPropAnno] || seq(i).isInstanceOf[TimeOp])
  //       // {
  //       //   psl += ";"
  //       // }
  //       if(seq(i).isInstanceOf[ImplicationOp] || seq(i).isInstanceOf[s_propBinOp] )
  //       {
  //         //println("??????")
  //         psl += "}"
  //         inSeq = false
  //       }
  //       else if(seq(i).isInstanceOf[s_propUnOp])
  //       {
  //         inSeq = false
  //       }
  //     }
  //     var temp:String = 
  //       seq(i) match {
  //       case AtmPropAnno(target) => rename2p(target)
  //       case up:s_propUnOp => 
  //       {
  //         up match {
  //           case NotOp() => "! "
  //           case FinallOp() => "F "
  //           case GlobalOp() => "G "
  //           case NextOp() => "X "
  //           case _ => throw new ClassCastException("Unsupported union property operator")
  //         }
  //       }
  //       case bp:s_propBinOp =>
  //       {
  //         bp match {
  //           case AndOp() => " & "
  //           case OrOp() => " | "
  //           case UntilOp() => " U "
  //           case _ => throw new ClassCastException("Unsupported union property operator")
  //         }
  //       }
  //       case bsp:s_seqBinOp =>
  //       {
  //         bsp match {
  //           case TimeOp(lb,ub) =>
  //           {
  //             if(i==seq.size - 1)
  //             {
  //               var s:String = ""
  //               if(inSeq)
  //                 s += ";"
  //               if(ub != -1)
  //                 s + "true[*" + lb + ".." + ub + "]"
  //               else 
  //                 s + "true[*" + lb + "]" + ";" + "true[*]"
  //             }
  //             else
  //             {
  //               val upperBounds:String = if(ub == -1) "$" else ub.toString()
  //             " ##[" + lb + ":" + upperBounds + "]"
  //             }
              
  //           }
  //           case OrSeqOp() => " | "
  //         }
  //       }
  //       case bup:s_seqUnOp =>
  //       {
  //         bup match {
  //           case RepetOp(lb,ub) =>
  //           {
  //             val upperBounds:String = if(ub == -1) "$" else ub.toString()
  //             " [*" + lb + ":" + upperBounds + "]"
  //           }
  //         }
  //       }
  //       case ImplicationOp() => "[]->"
  //       case Leftbraket() => 
  //       {
  //         var n = 1
  //         var right = -1
  //         var ind = i + 1
  //         while( n!=0 && ind < seq.size)
  //         {
  //           if(seq(ind).isInstanceOf[Leftbraket])
  //             n += 1
  //           else if(seq(ind).isInstanceOf[Rightbraket])
  //             n -= 1
  //           if(n==0)
  //             right = ind
  //           ind += 1
  //         }  
  //         if(n!=0)
  //         {
  //           println(s"right: $right")
  //           println(s"i: $i")
  //           throw new RuntimeException("Brakets don't match!")
  //         }
  //         else
  //         {
  //           println(s"right: $right")
  //           println(s"i: $i")
  //           val wrapedSeq = seq.slice(i+1,right)
  //           println(s"wrapedSeq: $wrapedSeq !!!")
  //           val isProperty = wrapedSeq.indexWhere{
  //             case p:svaElementAnno => p.isInstanceOf[s_propOp] || p.isInstanceOf[ImplicationOp] 
  //           }
  //           //Be careful , i will always increasing
  //           i = right
  //           if(isProperty == -1 && !inSeq)
  //           {
  //             inSeq = true
  //             "{ (" + toPSL(wrapedSeq, rename2p, inSeq, false) + ")"
  //           }
  //           else if(isProperty != -1 && !inSeq)
  //           {
  //             //inSeq = true
  //             "(" + toPSL(wrapedSeq, rename2p, inSeq, true) + ")"
  //           }
  //           else if(isProperty == -1 && inSeq)
  //           {
  //             //inSeq = true
  //             "(" + toPSL(wrapedSeq, rename2p, inSeq, false) + ")"
  //           }
  //           else
  //           {
  //             throw new RuntimeException("Wrong assertion!")
  //           }
  //         }
  //       }
  //     }
  //     psl += temp
  //     if(i==lastSeqEle & inSeq & needEndBrace)
  //     {
  //       psl += "}!"
  //       inSeq = false
  //     }
  //     i += 1
  //   }
  //   psl

  // }



  // @throws(classOf[RuntimeException])
  // def toSVATree(seq:Seq[svaElementAnno]): SVANode = {
  //   if( seq.isEmpty )
  //   {
  //     null
  //   }
  //   else if(seq.size == 1)
  //   {
  //     seq(0) match {
  //       case AtmPropAnno(target) => SVANode(AtmPropAnno(target), null, null)
  //       case _ => throw new ClassCastException("Unsupported class")
  //     }
  //   }
  //   else{
  //     seq(0) match {
  //       case p:s_propUnOp => SVANode(p,toSVATree(seq.slice(1,seq.size)),null,true)
  //       case Leftbraket() => {
  //         var n = 1
  //         var bre = false
  //         var left = -1
  //         for(i <- 1 until seq.size)
  //         {
  //           if(n != 0)
  //           {
  //             if(seq(i).isInstanceOf[Leftbraket])
  //               n = n + 1
  //             else if(seq(i).isInstanceOf[Rightbraket])
  //               n = n - 1
  //             if(n == 0)
  //               left = i
  //           }
  //         }
  //         println(s"right: $left")
  //         toSVATree(seq.slice(1,left))
  //       }
  //       case Rightbraket() => {println("mistake!"); null}
  //       // case AtmPropAnno(target) => SVANode(AtmPropAnno(target), null, null)
  //       case _ => {
  //         //pay attention
  //         //there exists some bug to be fixed!!!
  //         //pay attention
  //         val firstImp = seq.indexWhere(_.isInstanceOf[ImplicationOp])
  //         if(firstImp != -1)
  //         {
  //           val pre = toSVATree(seq.slice(0,firstImp))
  //           val post = toSVATree(seq.slice(firstImp+1,seq.size))
  //           if(pre.isProperty)
  //             throw new ClassCastException("unsupported syntax")
  //           else
  //             SVANode(ImplicationAnno(), pre, post, true)
  //         }
  //         else
  //         {
  //           //priority: or
  //           /*val lastRepet = seq.lastIndexWhere(_.isInstanceOf[RepetAnno])
  //           if(lastRepet == -1)
  //           {
  //             val firstTime = seq.indexWhere(_.isInstanceOf[TimeOpAnno])
  //             println(firstTime)
  //             if(firstTime < seq.size)
  //               SVANode(seq(firstTime), toSVATree(seq.slice(0,firstTime)), toSVATree(seq.slice(firstTime+1,seq.size)))
  //             else
  //               SVANode(seq(firstTime), toSVATree(seq.slice(0,firstTime)), null)
  //           }
  //           else
  //           {
  //             SVANode(RepetAnno(), toSVATree(seq.slice(0,lastRepet)), toSVATree(seq.slice(lastRepet+1,seq.size)))
  //           }*/
  //         }
  //       }
  //     }
  //   }
  // }

  def generateMap2p(seq:Seq[svaElementAnno]) : Map[Target,String] =
  {
    var i:Int = 0
    val temp = seq.collect{case AtmPropAnno(target) => target}.distinct
    temp.map(a => a->("p" + {i+=1; i})).toMap
  }

  // @throws(classOf[RuntimeException])
  // def toPSL(syntaxTree: SVANode, rename2p: Map[Target,String]) : String = 
  // {
  //   val leftPSL = if(syntaxTree.left == null) "" else toPSL(syntaxTree.left,rename2p)
  //   val rightPSL = if(syntaxTree.right == null) "" else toPSL(syntaxTree.right,rename2p)
  //   syntaxTree.ele match {
  //     case AtmPropAnno(target) => rename2p(target)
  //     case up:s_propUnOp => 
  //     {
  //       val op:String = up match {
  //         case NotOp() => "! "
  //         case FinallOp() => "F "
  //         case GlobalOp() => "G "
  //         case NextOp() => "X "
  //         case _ => throw new ClassCastException("Unsupported union property operator")
  //       }
  //       val ret = 
  //         if(!syntaxTree.left.isProperty)
  //           op + "{" + leftPSL + "}"
  //         else
  //           op + leftPSL
  //       ret
  //     }
  //     case TimeOp(lb,ub) =>
  //     {
  //       var s:String = ""
  //       if(syntaxTree.left != null)
  //         s = leftPSL + ";"
  //       if(ub != -1)
  //         s + "true[*" + lb + ".." + ub + "]" +";"+ rightPSL
  //       else 
  //         s + "true[*" + lb + "]" + ";" + "true[*]" +";"+ rightPSL
  //     }
  //     case ImplicationOp() => "{" + leftPSL + "}" +"[]->" + rightPSL
  //   }
  // }


  def SVAAnno2PSL(s: svaSeqAnno) : Tuple3[String, Map[String,Target], Target] = 
  {
    val elementSVA = s.toElementSeq().toSeq
    println(s"elementSVA: $elementSVA")
    val resetAn = elementSVA.filter(_.isInstanceOf[ResetAnno])
    assert(resetAn.size == 1,"only allow one reset signal")
    val remainSVA = elementSVA.filter(!_.isInstanceOf[ResetAnno])
    println(s"remainSVA: $remainSVA")
    val target2p = svaSeqAnno.generateMap2p(remainSVA)
    val p2target = target2p.toSeq.map{case Tuple2(k,v) => Tuple2(v,k)}.toMap
    println(p2target)
    //val syntaxTree = svaSeqAnno.toSVATree(remainSVA)
    //println(svaSeqAnno.toSVATree(remainSVA))
    //val psl = if(!syntaxTree.isProperty) "!{" + svaSeqAnno.toPSL(syntaxTree,target2p) + "}"  else "!" + svaSeqAnno.toPSL(syntaxTree,target2p)
    val psl = "! ( G " + svaSeqAnno.toPSL(remainSVA,target2p,false) + " ) "
    //val psl = "!" + svaSeqAnno.toPSL(syntaxTree,target2p)
    println(s"psl: $psl")
    (psl,p2target,resetAn(0).asInstanceOf[ResetAnno].target)
  }
}

case class svaSeqAnno(ttargets: Seq[Seq[svaElementAnno]]) extends MultiTargetAnnotation{
  /*println(ttargets.toSeq.toString)
  println(ttargets.map(Seq(_)).toSeq.toString)*/
  //ttargets.filter(_.isInstanceOf[AtmPropAnno])
  override val targets: Seq[Seq[Target]] = ttargets.filter(_.isInstanceOf[AtmPropAnno]).map(t => Seq(t.asInstanceOf[AtmPropAnno].target))
  //override duplication, but don't use it!
  //override def update(renames: RenameMap): Seq[Annotation] = Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))
  //def duplicate(n: T): Annotation
  override def duplicate(n: Seq[Seq[Target]]): Annotation =  
  { 
    val tt:Seq[Target] = n.flatten 
    this.copy(Seq(Seq(AtmPropAnno(tt(0))))) 
  }
  
  //Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))
  override def update(renames: RenameMap) :Seq[Annotation]= 
  {
    Seq(this.copy(ttargets.map(
      ts => ts.flatMap
      {
        case AtmPropAnno(target) => renames(target).map{AtmPropAnno(_)}
        case ResetAnno(target) => renames(target).map{ResetAnno(_)}
        case a => Seq(a)
      }
  )))
  }

  /*sameModule should be after flat, since we hope ttargets is like Seq(Seq(svaElementAnno), Seq(svaElementAnno), ...)  */
  def sameModule() : Boolean = 
  {
    val temp = this.ttargets.flatten.collect{case a:AtmPropAnno => a.target.toTarget.moduleOpt}.distinct
    //println(temp)
    temp.size <= 1
  }

  private def crossJoin[T](list: Seq[Seq[T]]): Seq[Seq[T]] =
    list match {
      case Nil      => Nil
      case x :: Nil => x.map(Seq(_))
      case x :: xs =>
        val xsJoin = crossJoin(xs)
        for {
          i <- x
          j <- xsJoin
        } yield {
          Seq(i) ++ j
        }
    }
  override def flat(): AnnotationSeq = crossJoin(ttargets).map(r => this.copy(r.map(Seq(_))))
  
  def toElementSeq(): Seq[svaElementAnno] = ttargets.flatMap(_.slice(0,1))
    //println("--------")
    /*ttargets.map{
      case Seq(AtmPropAnno(target)) => 
        {
          if(!renames(target).isEmpty)
          {
            println(target)
            println("***")
            println(renames(target))
          }
        }
        case Seq(TimeOp(lowerBounds, upperBounds)) => 
          {
            println("Timep")
          }
    }*/
    
    /*println("--------")
    ttargets.map{
      case AtmPropAnno(target) => 
        {
          if(!renames(target).isEmpty)
          {
            println(target)
            println("***")
            println(renames(target))
          }
        }
        case TimeOp(lowerBounds, upperBounds) => 
          {
            println("Timep")
          }
    }
    Seq(this.copy(ttargets))*/
  

  /*override def update(renames: RenameMap) = 
  {
    val tttemp : Seq[AtmPropAnno] = ttargets map  
    {
      case a:AtmPropAnno => {
        //println(t)
        //println(t.target.isInstanceOf[Target])
        //AtmPropAnno(renames(t.asInstanceOf[AtmPropAnno].target))
        val xx :Target = t.target
        val xxx = renames.get(xx.asInstanceOf[Target])
        a
      }
      case t:TimeOp =>
      {

      }
    }

    //println(ttemp)
    /*ttargets.map{
      case AtmPropAnno(ap:Target) => 
        {
          println(ap)
          //val ttt :firrtl.annotations.Target = t1.asInstanceOf[AtmPropAnno].target
          val aap :firrtl.annotations.Target = ap.toTarget
          AtmPropAnno(ap)
          //AtmPropAnno(renames(aap))
        }
      case TimeOp(lb,up) =>TimeOp(lb,up)
    }*/
    Seq(this.copy(ttemp))
  }*/
}

