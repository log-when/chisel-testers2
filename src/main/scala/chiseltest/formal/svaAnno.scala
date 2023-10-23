// important: solve problems casued by extra clock information and module information...
// important: reconsider the semantics of reset: G (!reset -> p)
package chiseltest.formal

import scala.collection.mutable
import chisel3._
import chisel3.experimental.{ChiselAnnotation,annotate,RunFirrtlTransform}
import firrtl.annotations.{Annotation, ReferenceTarget, SingleTargetAnnotation, Target, MultiTargetAnnotation,NoTargetAnnotation}
import firrtl.options.StageUtils
import firrtl.RenameMap
import org.json4s.JValue
import firrtl._
import firrtl.ir.Expression

import scala.collection.Traversable
import scala.reflect.runtime.{universe => ru}
import javax.swing.text.html.HTMLEditorKit.Parser
import scala.util.parsing.combinator._

// class(trait) hierarch:
// sva_op(only for parser) - seq_op -repe_op, time_delay_op

// sva_node: for parser and annotation sequence
//  -sva_seq: describe seq in parser
//  -sva_prop: describe prop in parser 
//  -svaElementAnno: describe the prop in annotation sequence
//    -un_op_node: uniary operator
//      +sva_seq: un_op_seq
//      +sva_pro: un_op_prop
//    -bin_op_node: binary operator
//      +sva_seq: bin_op_seq
//      +sva_pro: bin_op_prop

//Only for parser, suffixed with "op" 
trait sva_op
trait seq_op extends sva_op
case class repe_op(lowerBound:Int, upperBound:Int) extends seq_op
case class time_delay_op(lowerBound:Int, upperBound:Int) extends seq_op

//For parser and annotation sequence
trait sva_node
{
  // For serialization: omit the information of its children
  def eraseChildren(): sva_node = this
  def setChildren(s: Seq[sva_node]): Unit = {}
  // Unfold the tree structure and return a sequence
  def treeSerialize(): Seq[sva_node] = 
  {
    this match
    {
      case n:atom_prop_node => Seq(this.eraseChildren())
      case n:bin_op_node[_,_] => 
      {
        val lSeq = n.lchild.treeSerialize()
        val rSeq = n.rchild.treeSerialize()
        Seq(n.eraseChildren()) ++ lSeq ++ rSeq
      }
      case n:un_op_node[_] => 
      {
        val lSeq = n.child.treeSerialize()
        Seq(n.eraseChildren()) ++ lSeq 
      }
      case n:constantTrue => Seq(constantTrue())
      case n:constantFalse => Seq(constantFalse())
      case n => {println(s"Unexcepted error? $n"); Seq(n.eraseChildren())}
    }
  }

  def treeDeSerialize(s:Seq[sva_node]): Tuple2[sva_node, Int] =
  {
    this match 
    {
      case n:atom_prop_anno => (this, 1)
      case n:constantTrue => (this, 1)
      case n:constantFalse => (this, 1)
      case n: un_op_node[_] => 
      {
        val (tChild, costLength) = s(0).treeDeSerialize(s.tail)
        this.setChildren(Seq(tChild))
        (this, costLength + 1)
      }  
      case n: bin_op_node[_,_] => 
      {
        val (tChild1, costLength1) = s(0).treeDeSerialize(s.tail)
        val (tChild2, costLength2) = s(costLength1).treeDeSerialize(s.slice(costLength1+1,s.size)) 
        this.setChildren(Seq(tChild1, tChild2))
        (this, costLength1 + costLength2 + 1)
      } 
    }
  }
}

trait svaElementAnno extends sva_node
{
  def toPSL(rename2p: Map[Target,String]): String
  def toSVA(f: Target => Expression): Seq[Any]
}
trait un_op_node[T<: sva_node] extends svaElementAnno
{
  var child: T
  def opPSLL: String
  def opPSLR: String
  def opSVAL: String
  def opSVAR: String
  override def eraseChildren(): sva_node = { child = null.asInstanceOf[T]; this }
  override def setChildren(s: Seq[sva_node])= 
  {
    assert(s.size == 1)
    this.child = s(0).asInstanceOf[T]
  }
  override def toPSL(rename2p: Map[Target,String]): String = "(" + opPSLL + child.asInstanceOf[svaElementAnno].toPSL(rename2p) + opPSLR +")" +
    ""
  override def toSVA(f: Target => Expression): Seq[Any] = Seq("(", opSVAL, child.asInstanceOf[svaElementAnno].toSVA(f) , opSVAR ,")")
}

trait bin_op_node[T1<: sva_node, T2<: sva_node] extends svaElementAnno
{
  var lchild: T1
  var rchild: T2
  def opPSL: String
  def opSVA: String

  override def eraseChildren(): sva_node = { lchild = null.asInstanceOf[T1]; rchild = null.asInstanceOf[T2]; this }
  override def setChildren(s: Seq[sva_node])= 
  {
    assert(s.size == 2)
    this.lchild = s(0).asInstanceOf[T1]
    this.rchild = s(1).asInstanceOf[T2]
  }

  // A strange error when using match-case 
  override def toPSL(rename2p: Map[Target,String]): String = 
  {
    if(this.isInstanceOf[over_impl_prop])
    {
      "({ "+ lchild.asInstanceOf[svaElementAnno].toPSL(rename2p) + " }" + opPSL + rchild.asInstanceOf[svaElementAnno].toPSL(rename2p) + ")"
    }
    else
    {
      "(" + lchild.asInstanceOf[svaElementAnno].toPSL(rename2p) + opPSL + rchild.asInstanceOf[svaElementAnno].toPSL(rename2p) + ")"
    }
  }

  override def toSVA(f: Target => Expression): Seq[Any] = 
  {
    if(this.isInstanceOf[over_impl_prop])
    {
      Seq(lchild.asInstanceOf[svaElementAnno].toSVA(f) , opSVA , rchild.asInstanceOf[svaElementAnno].toSVA(f))
    }
    else
    {
      Seq(lchild.asInstanceOf[svaElementAnno].toSVA(f) , opSVA , rchild.asInstanceOf[svaElementAnno].toSVA(f))
    }
  }
}

trait sva_seq extends sva_node
trait un_op_seq extends un_op_node[sva_seq] with sva_seq
trait bin_op_seq extends bin_op_node[sva_seq, sva_seq] with sva_seq

case class constantTrue() extends sva_seq with svaElementAnno
{
  override def toPSL(rename2p: Map[Target,String]): String = "1" 
  override def toSVA(f: Target => Expression): Seq[Any] = Seq("1")
}
case class constantFalse() extends sva_seq with svaElementAnno
{
  override def toPSL(rename2p: Map[Target,String]): String = "0" 
  override def toSVA(f: Target => Expression): Seq[Any] = Seq("0")
}
case class atom_prop_node(signal:Bool) extends sva_seq
case class atom_prop_anno(signal:ReferenceTarget) extends sva_seq with svaElementAnno
{
  override def toPSL(rename2p: Map[Target,String]): String = rename2p(signal) 
  override def toSVA(f: Target => Expression): Seq[Any] = Seq(f(signal))
}
case class repe_seq(lowerBound:Int, upperBound:Int, var child:sva_seq) extends un_op_seq
{
  override def opPSLL: String = "" 
  override def opPSLR: String = 
  {
    val upperBounds:String = if(upperBound == -1) "$" else upperBound.toString()
    "[*" + lowerBound + ":" + upperBounds + "]"
  }

  override def opSVAL: String = opPSLL
  override def opSVAR: String = opPSLR
}
case class time_delay_seq(lowerBound:Int, upperBound:Int, var lchild:sva_seq, var rchild:sva_seq) extends bin_op_seq
{
  override def opPSL: String = 
  {
    val upperBounds:String = if(upperBound == -1) "$" else upperBound.toString()
    "##[" + lowerBound + ":" + upperBounds + "]"
  }
  override def opSVA: String = opPSL
}
case class and_seq(var lchild:sva_seq, var rchild:sva_seq) extends bin_op_seq
{
  override def opPSL: String = " && "
  override def opSVA: String = " and "
}
case class or_seq(var lchild:sva_seq, var rchild:sva_seq) extends bin_op_seq
{
  override def opPSL: String = " || "
  override def opSVA: String = " or "
}


trait sva_pro extends sva_node
trait un_op_pro extends un_op_node[sva_pro] with sva_pro
trait bin_op_pro extends bin_op_node[sva_pro, sva_pro] with sva_pro

case class not_prop(var child:sva_pro) extends un_op_pro
{
  override def opPSLL: String = "! "
  override def opPSLR: String = ""
  override def opSVAL: String = "not"
  override def opSVAR: String = opPSLR
}
case class next_prop(var child:sva_pro) extends un_op_pro
{
  override def opPSLL: String = "X"
  override def opPSLR: String = ""
  override def opSVAL: String = "nexttime"
  override def opSVAR: String = opPSLR
}
case class and_prop(var lchild:sva_pro, var rchild:sva_pro) extends bin_op_pro
{
  override def opPSL: String = " && "
  override def opSVA: String = " and "
}
case class or_prop(var lchild:sva_pro, var rchild:sva_pro) extends bin_op_pro
{
  override def opPSL: String = " || "
  override def opSVA: String = " or "
}
case class until_prop(var lchild:sva_pro, var rchild:sva_pro) extends bin_op_pro
{
  override def opPSL: String = " U "
  override def opSVA: String = "until"
}
case class impl_prop(var lchild:sva_pro, var rchild:sva_pro) extends bin_op_pro
{
  override def opPSL: String = " -> "
  override def opSVA: String = " implies "
}
case class glo_prop(var child:sva_pro) extends un_op_pro
{
  override def opPSLL: String = "G "
  override def opPSLR: String = ""

  override def opSVAL: String = "always"
  override def opSVAR: String = ""
}
case class fina_prop(var child:sva_pro) extends un_op_pro
{
  override def opPSLL: String = "F "
  override def opPSLR: String = ""

  override def opSVAL: String = "s_eventually"
  override def opSVAR: String = ""
}

case class over_impl_prop(var lchild:sva_seq, var rchild:sva_pro) extends bin_op_node[sva_seq, sva_pro] with sva_pro
{
  override def opPSL: String = " []-> "
  override def opSVA: String = " |-> "
}
case class prompt_prop(var child:sva_seq) extends un_op_node[sva_seq] with sva_pro
{
  override def opPSLL: String = ""
  override def opPSLR: String = ""

  override def opSVAL: String = ""
  override def opSVAR: String = ""
  override def toPSL(rename2p: Map[Target,String]): String = 
  {
    "(" + "{" + child.asInstanceOf[svaElementAnno].toPSL(rename2p) + "}" +")" 
  }
}
// case class NoneOp() extends sva_node


// case class NoneOp() extends sva_node
// case class ResetAnno(target:Target) extends sva_node
// {
//   override def toPSL(rename2p: Map[Target,String]): String = {println("Misuse this API"); ""}
//   override def toSVA(f: Target => Expression): Seq[Any] = {println("Misuse this API"); ""}
// }

trait targetAnno extends sva_node
{
  val target: Target
}

case class ResetAnno(target:Target) extends targetAnno
case class EnableAnno(target:Target) extends targetAnno
case class ClockAnno(target:Target) extends targetAnno
case class ModuleAnno(target:Target) extends targetAnno

class sva_tree(o:Object) extends JavaTokenParsers {
  def prop: Parser[sva_pro] = 
  (
      prop6 ^^{case p => p}
  )
  def prop6: Parser[sva_pro] =
  (
      "G"~>prop6                    ^^ {case p:sva_pro => println(s"prop5: $p"); glo_prop(p)}
    | "F"~>prop6                    ^^ {case p:sva_pro => println(s"prop5: $p"); fina_prop(p)}
    | prop5                         ^^ {case p:sva_pro => println(s"prop5: $p"); p}
  )
  def prop5: Parser[sva_pro] =
  (
    seq~"|->"~prop6                 ^^ {case ~(~(p1,o),p2) =>  println(s"prop4: $p1 $p2"); over_impl_prop(p1,p2)}
    | prop4                         ^^ {case p:sva_pro => println(s"prop4: $p"); p}
  )
  def prop4: Parser[sva_pro] =
  (
      prop3~"U"~prop6               ^^ {case ~(~(p1,o),p2) =>  println(s"prop3: $p1 $p2"); until_prop(p1,p2)}
    | prop3~"->"~prop6              ^^ {case ~(~(p1,o),p2) =>  println(s"prop3: $p1 $p2"); impl_prop(p1,p2)}
    | prop3                         ^^ {case p:sva_pro =>  p}
  )
  def prop3: Parser[sva_pro] =
  (
    prop2~opt("||"~prop6)           ^^ {case ~(p1,Some(~(o,p2))) =>  or_prop(p1,p2)
                                      case ~(p,None) => p}
  )
  def prop2: Parser[sva_pro] =
  (
    prop1~opt("&&"~prop6)           ^^ {case ~(p1,Some(~(o,p2))) =>  and_prop(p1,p2)
                                      case ~(p,None) => p}
  )
  def prop1: Parser[sva_pro] = 
  (
      "!"~>prop1 ^^ {case p:sva_pro => println(s"prop1: $p");not_prop(p)}  
    | "X"~>prop1 ^^ {case p:sva_pro => println(s"prop1: $p");next_prop(p)}
    | seq       ^^ {case s:sva_seq => println(s"prop1: $s");prompt_prop(s)}
    | "("~>prop<~")" ^^{case p:sva_pro => println(s"prop1: $p");p}
    // |  "!"~>prop ^^ {case p:sva_pro => println(s"prop1: $p");not_prop(p)}  
    // | "X"~>prop ^^ {case p:sva_pro => println(s"prop1: $p");next_prop(p)}
    // | "G"~>prop ^^ {case p:sva_pro => println(s"prop1: $p");glo_prop(p)}
  )

  // Use seq1,seq2 to maintain the precedence: ## > && > or
  // Unary sequence operator repetition has highest priority (in repe) 
  def seq: Parser[sva_seq] =
  (
    opt(time_delay)~seq3         ^^ {case ~(Some(t),s) =>  time_delay_seq(t.lowerBound,t.upperBound,constantTrue(),s)
                                        case ~(None,s) => s
                                            }
  )
  def seq3: Parser[sva_seq] = 
  (
    seq2~opt("||"~seq)              ^^ {case ~(s1,Some(~(o,s2))) =>  or_seq(s1,s2)
                                      case ~(s,None) => println(s"seq2: $s"); s
                                      case x => println(x); new atom_prop_node(false.asBool)}
  )
  def seq2: Parser[sva_seq] = 
  (
    seq1~opt("&&"~seq2)             ^^ {case ~(s1,Some(~(o,s2))) =>  and_seq(s1,s2)
                                      case ~(s,None) => s
                                      case x => println(x); new atom_prop_node(false.asBool)}
    // seq1 ^^ {case p => println(s"seq: $p"); p}
  )
  def seq1: Parser[sva_seq] = 
  (
    repe~opt(time_delay~seq1)       ^^ {case ~(r,Some(~(t,s))) =>  time_delay_seq(t.lowerBound,t.upperBound,r,s)
                                      case ~(r,None) => r
                                      case x => println(x); new atom_prop_node(false.asBool)}
  )
    
  //##m, ##[m:n], ##[*], ##[+]
  def time_delay: Parser[time_delay_op] = 
  (
    // "##"~(wholeNumber | "["~(wholeNumber~":"~wholeNumber | wholeNumber~":"~"$" | "*" | "+")~"]") 
      "##"~>tuple2                  ^^ {t => time_delay_op(t._1,t._2)}
    | "##"~"["~>tuple2<~"]"         ^^ {t => time_delay_op(t._1,t._2)}
  )
  def repe: Parser[sva_seq] = 
  (
      atom_prop~opt(repe_range)     ^^ {case ~(ap,Some(r)) => println(s"repe $ap, $r"); repe_seq(r.lowerBound,r.upperBound,ap)
                                      case ~(ap,None)    => println(s"repe $ap"); ap}
    | "("~>seq~")"~opt(repe_range)  ^^ {case ~(~(seq,")"),Some(r)) => repe_seq(r.lowerBound,r.upperBound,seq)
                                      case ~(~(seq,")"),None) => seq
                                      case _ => println("what happened?"); new atom_prop_node(false.asBool)}
  )
  //[*m], [*m:n], [*m:$], [*], [+] 
  def repe_range: Parser[repe_op] = 
  (
      "["~> tuple2 <~"]"            ^^ {t => repe_op(t._1,t._2)}    
    | "["~"*"~> tuple2 <~"]"        ^^ {t => repe_op(t._1,t._2)}
  )   
  def tuple2: Parser[Tuple2[Int,Int]] =
  (
      wholeNumber~":"~wholeNumber   ^^ {case ~(~(m,":"),n) => Tuple2(m.toInt,n.toInt)}
                                    // case x => throw new Exception("Illegal input!") 
    | wholeNumber~":"~"$"           ^^ {case ~(~(m,":"),"$") => Tuple2(m.toInt, -1)}
    | wholeNumber                   ^^ (x => Tuple2(x.toInt, x.toInt))
    | "+"                           ^^ (x => Tuple2(1, -1))
    | "*"                           ^^ (x => Tuple2(0, -1))
  )

  // handling reserved keyword  
  def atom_prop: Parser[atom_prop_node] = 
  (
    """[a-zA-Z_]\w+""".r         ^^ {x => 
                                        {
                                          println(s"??? $x")
                                          //Notice: no exception handling
                                          val variable = o.getClass.getMethod(x).invoke(o)
                                          atom_prop_node(variable.asInstanceOf[Bool])
                                        }}
    | """[a-zA-Z_&&[^XFG]]""".r  ^^ {x => 
                                        {
                                          println(s"??? $x")
                                          //Notice: no exception handling
                                          val variable = o.getClass.getMethod(x).invoke(o)
                                          atom_prop_node(variable.asInstanceOf[Bool])
                                        }}
  )  
}

trait svaStmt
case object svaAssertStmt extends svaStmt
case object svaAssumeStmt extends svaStmt

object svaAnno
{
  def svaAssert(o:Object, e:Bool) =
  {
    val res = o.asInstanceOf[Module].reset
    val en = !res.asBool
    println(s"en: $en")
    dontTouch(en)
    val clo = o.asInstanceOf[Module].clock
    val mod = o.asInstanceOf[Module]

    val svaSeq =  Seq(atom_prop_node(e))
    println(svaSeq)

    svaSeq.foreach{
      case a: atom_prop_node => dontTouch(a.signal) 
      case b => 
    }
    annotate(new ChiselAnnotation {
      // Conversion to FIRRTL Annotation 
      override def toFirrtl: Annotation = 
      {
        val svaanotation : Seq[Seq[sva_node]] = svaSeq map {
          case atom_prop_node(ap) => Seq(atom_prop_anno(ap.toTarget))
          case otherOp: svaElementAnno => Seq(otherOp)
        } 
        println(s"svaAnnotation: ${svaanotation.toSeq}")
        new svaAssertAnno(svaanotation:+Seq(ResetAnno(res.toTarget)):+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
        // new svaAnno(svaanotation:+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
      }
    })
  }

  def svaAssert(o:Object, s:String) =
  {
    val res = o.asInstanceOf[Module].reset
    val en = !res.asBool
    println(s"en: $en")
    dontTouch(en)
    val clo = o.asInstanceOf[Module].clock
    dontTouch(clo)
    val mod = o.asInstanceOf[Module]
    val svaTree = new sva_tree(o)
    println(svaTree.prop6)
    val syntaxTree = svaTree.parseAll(svaTree.prop6, s)
    println(s"$res, $syntaxTree")
    val svaSeq = syntaxTree.get.treeSerialize()
    println(svaSeq)

    svaSeq.foreach{
      case a: atom_prop_node => dontTouch(a.signal) 
      case b => 
    }
    annotate(new ChiselAnnotation {
      // Conversion to FIRRTL Annotation 
      override def toFirrtl: Annotation = 
      {
        val svaanotation : Seq[Seq[sva_node]] = svaSeq map {
          case atom_prop_node(ap) => Seq(atom_prop_anno(ap.toTarget))
          case otherOp: svaElementAnno => Seq(otherOp)
        } 
        println(s"svaAnnotation: ${svaanotation.toSeq}")
        new svaAssertAnno(svaanotation:+Seq(ResetAnno(res.toTarget)):+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
        // new svaAnno(svaanotation:+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
      }
    })
  }

  def svaAssume(o:Object, s:String) =     
  {
    val res = o.asInstanceOf[Module].reset
    val en = !res.asBool
    println(s"en: $en")
    dontTouch(en)
    val clo = o.asInstanceOf[Module].clock
    val mod = o.asInstanceOf[Module]
    val svaTree = new sva_tree(o)
    println(svaTree.prop6)
    val syntaxTree = svaTree.parseAll(svaTree.prop6, s)
    println(s"$res, $syntaxTree")
    val svaSeq = syntaxTree.get.treeSerialize()
    println(svaSeq)

    svaSeq.foreach{
      case a: atom_prop_node => dontTouch(a.signal) 
      case b => 
    }
    annotate(new ChiselAnnotation {
      // Conversion to FIRRTL Annotation 
      override def toFirrtl: Annotation = 
      {
        val svaanotation : Seq[Seq[sva_node]] = svaSeq map {
          case atom_prop_node(ap) => Seq(atom_prop_anno(ap.toTarget))
          case otherOp: svaElementAnno => Seq(otherOp)
        } 
        // println("svaAnnotation: $svaanotation")
        println(s"ClockAnno: ${ClockAnno(clo.toTarget)}")
        new svaAssumeAnno(svaanotation:+Seq(ResetAnno(res.toTarget)):+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
        // new svaAnno(svaanotation:+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
      }
    })
  }
  
  def generateMap2p(seq:Seq[sva_node]) : Map[Target,String] =
  {
    var i:Int = 0
    val temp = seq.collect{
      case t: targetAnno => t.target
      case atom_prop_anno(target) => target
    }.distinct
    temp.map(a => a->("p" + {i+=1; i})).toMap
  }

  def SVAAnno2PSL(s: svaAnno): Tuple4[String, Map[String,Target], Target, svaStmt] = 
  {
    val elementSVA = s.toElementSeq().toSeq
    println(s"elementSVA: $elementSVA")
    val resetAn = elementSVA.filter(_.isInstanceOf[ResetAnno])
    // assert(resetAn.size == 1,"only allow one reset signal")

    val remainSVA = elementSVA.filter(!_.isInstanceOf[targetAnno])
    println(s"remainSVA: $remainSVA")
    val target2p = svaAnno.generateMap2p(remainSVA)
    val p2target = target2p.toSeq.map{case Tuple2(k,v) => Tuple2(v,k)}.toMap
    // val seq_ =mutable.Seq(remainSVA:_*)
    val deSeri = remainSVA(0).treeDeSerialize(remainSVA.tail)
    println(s"deserialization: ${deSeri}")

    // distinguish assert with assume, assert statement need to be negated
    val isAssert = s.isInstanceOf[svaAssertAnno]
    val neg = if (isAssert) "! " else ""
    val psl = neg +"G(" + deSeri._1.asInstanceOf[svaElementAnno].toPSL(target2p) + ") "

    //val psl = "!" + svaAnno.toPSL(syntaxTree,target2p)
    println(s"psl: $psl")
    println(s"$p2target")
    val stmt = if (isAssert) svaAssertStmt else svaAssumeStmt
    (psl,p2target,resetAn(0).asInstanceOf[ResetAnno].target,stmt)
  }
}



case class svaAssumeAnno(ttargets: Seq[Seq[sva_node]]) extends svaAnno
case class svaAssertAnno(ttargets: Seq[Seq[sva_node]]) extends svaAnno

trait svaAnno extends MultiTargetAnnotation{
  /*println(ttargets.toSeq.toString)
  println(ttargets.map(Seq(_)).toSeq.toString)*/
  //ttargets.filter(_.isInstanceOf[atom_prop_anno])
  val ttargets: Seq[Seq[sva_node]]
  def copy(ts: Seq[Seq[sva_node]]) = {
    this match {
      case svaAssertAnno(ttargets) => svaAssertAnno(ts)
      case svaAssumeAnno(ttargets) => svaAssumeAnno(ts)
    } 
  }

  override val targets: Seq[Seq[Target]] = ttargets.filter(_.isInstanceOf[atom_prop_anno]).map(t => Seq(t.asInstanceOf[atom_prop_anno].signal))
  //override duplication, but don't use it!
  //override def update(renames: RenameMap): Seq[Annotation] = Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))
  //def duplicate(n: T): Annotation
  override def duplicate(n: Seq[Seq[Target]]): Annotation =  
  { 
    val tt:Seq[Target] = n.flatten 
    this.copy(Seq(Seq(constantTrue()))) 
  }
  
  //Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))
  override def update(renames: RenameMap) :Seq[Annotation]= 
  {
    Seq(this.copy(ttargets.map(
      ts => ts.flatMap
      {
        case atom_prop_anno(target) => renames(target).map{x => atom_prop_anno(x.asInstanceOf[ReferenceTarget])}
        case ResetAnno(target) => renames(target).map{ResetAnno(_)}
        case ClockAnno(target) => {println(s"clock update: ${ClockAnno(target)}, ${renames(target).map{ClockAnno(_)}} "); renames(target).map{ClockAnno(_)}}
        case EnableAnno(target) => renames(target).map{EnableAnno(_)}
        case ModuleAnno(target) => renames(target).map{ModuleAnno(_)}
        case a => Seq(a)
      }
  )))
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
  
  def toElementSeq(): Seq[sva_node] = ttargets.flatMap(_.slice(0,1))
}



case class target2ExprAnno(getMap: Map[Target,Expression]) extends NoTargetAnnotation

