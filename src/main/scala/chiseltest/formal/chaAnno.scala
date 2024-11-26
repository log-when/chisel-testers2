// added in cha: define chaAssertion and parse 
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
// cha_op(only for parser) - seq_op -repe_op, time_delay_op

// cha_node: for parser and annotation sequence
//  -cha_seq: describe seq in parser
//  -cha_prop: describe prop in parser 
//  -chaElementAnno: describe the prop in annotation sequence
//    -un_op_node: uniary operator
//      +cha_seq: un_op_seq
//      +cha_pro: un_op_prop
//    -bin_op_node: binary operator
//      +cha_seq: bin_op_seq
//      +cha_pro: bin_op_prop

//Only for parser, suffixed with "op" 
trait cha_op
trait seq_op extends cha_op
case class repe_op(lowerBound:Int, upperBound:Int) extends seq_op
case class time_delay_op(lowerBound:Int, upperBound:Int) extends seq_op

//For parser and annotation sequence
trait cha_node
{
  // For serialization: omit the information of its children
  def eraseChildren(): cha_node = this
  def setChildren(s: Seq[cha_node]): Unit = {}
  // Unfold the tree structure and return a sequence
  def treeSerialize(): Seq[cha_node] = 
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

  def treeDeSerialize(s:Seq[cha_node]): Tuple2[cha_node, Int] =
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

trait chaElementAnno extends cha_node
{
  def toPSL(rename2p: Map[Target,String]): String
  def toSVA(f: Target => Expression): Seq[Any]
}
trait un_op_node[T<: cha_node] extends chaElementAnno
{
  var child: T
  def opPSLL: String
  def opPSLR: String
  def opSVAL: String
  def opSVAR: String
  override def eraseChildren(): cha_node = { child = null.asInstanceOf[T]; this }
  override def setChildren(s: Seq[cha_node])= 
  {
    assert(s.size == 1)
    this.child = s(0).asInstanceOf[T]
  }
  override def toPSL(rename2p: Map[Target,String]): String = "(" + opPSLL + child.asInstanceOf[chaElementAnno].toPSL(rename2p) + opPSLR +")" +
    ""
  override def toSVA(f: Target => Expression): Seq[Any] = Seq("(", opSVAL, child.asInstanceOf[chaElementAnno].toSVA(f) , opSVAR, ")")
}

trait bin_op_node[T1<: cha_node, T2<: cha_node] extends chaElementAnno
{
  var lchild: T1
  var rchild: T2
  def opPSL: String
  def opSVA: String

  override def eraseChildren(): cha_node = { lchild = null.asInstanceOf[T1]; rchild = null.asInstanceOf[T2]; this }
  override def setChildren(s: Seq[cha_node])= 
  {
    assert(s.size == 2)
    this.lchild = s(0).asInstanceOf[T1]
    this.rchild = s(1).asInstanceOf[T2]
  }

  // A strange error when using match-case 
  override def toPSL(rename2p: Map[Target,String]): String = 
  {
    if(this.isInstanceOf[over_impl_prop] || this.isInstanceOf[noover_impl_prop])
    {
      "({ "+ lchild.asInstanceOf[chaElementAnno].toPSL(rename2p) + " }" + opPSL + rchild.asInstanceOf[chaElementAnno].toPSL(rename2p) + ")"
    }
    else
    {
      "(" + lchild.asInstanceOf[chaElementAnno].toPSL(rename2p) + opPSL + rchild.asInstanceOf[chaElementAnno].toPSL(rename2p) + ")"
    }
  }

  override def toSVA(f: Target => Expression): Seq[Any] = 
  {
    Seq("(", lchild.asInstanceOf[chaElementAnno].toSVA(f) , opSVA , rchild.asInstanceOf[chaElementAnno].toSVA(f), ")")
  }
}

trait cha_seq extends cha_node
trait un_op_seq extends un_op_node[cha_seq] with cha_seq
trait bin_op_seq extends bin_op_node[cha_seq, cha_seq] with cha_seq

case class constantTrue() extends cha_seq with chaElementAnno
{
  override def toPSL(rename2p: Map[Target,String]): String = "1" 
  override def toSVA(f: Target => Expression): Seq[Any] = Seq("1")
}
case class constantFalse() extends cha_seq with chaElementAnno
{
  override def toPSL(rename2p: Map[Target,String]): String = "0" 
  override def toSVA(f: Target => Expression): Seq[Any] = Seq("0")
}
case class atom_prop_node(signal:Bool) extends cha_seq
{
  def unary_! : atom_prop_node = {
    atom_prop_node(!signal)
  }
}
case class atom_prop_anno(signal:ReferenceTarget) extends cha_seq with chaElementAnno
{
  override def toPSL(rename2p: Map[Target,String]): String = rename2p(signal) 
  override def toSVA(f: Target => Expression): Seq[Any] = Seq(f(signal))
}
case class repe_seq(lowerBound:Int, upperBound:Int, var child:cha_seq) extends un_op_seq
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
case class time_delay_seq(lowerBound:Int, upperBound:Int, var lchild:cha_seq, var rchild:cha_seq) extends bin_op_seq
{
  override def opPSL: String = 
  {
    val upperBounds:String = if(upperBound == -1) "$" else upperBound.toString()
    " ##[" + lowerBound + ":" + upperBounds + "] "
  }
  override def opSVA: String = opPSL
}
case class and_seq(var lchild:cha_seq, var rchild:cha_seq) extends bin_op_seq
{
  override def opPSL: String = " && "
  override def opSVA: String = " and "
}
case class or_seq(var lchild:cha_seq, var rchild:cha_seq) extends bin_op_seq
{
  override def opPSL: String = " || "
  override def opSVA: String = " or "
}


trait cha_pro extends cha_node
trait un_op_pro extends un_op_node[cha_pro] with cha_pro
trait bin_op_pro extends bin_op_node[cha_pro, cha_pro] with cha_pro

case class not_prop(var child:cha_pro) extends un_op_pro
{
  override def opPSLL: String = "! "
  override def opPSLR: String = ""
  override def opSVAL: String = "not "
  override def opSVAR: String = opPSLR
}
case class next_prop(var child:cha_pro) extends un_op_pro
{
  override def opPSLL: String = "X "
  override def opPSLR: String = ""
  override def opSVAL: String = "nexttime "
  override def opSVAR: String = opPSLR
}
case class and_prop(var lchild:cha_pro, var rchild:cha_pro) extends bin_op_pro
{
  override def opPSL: String = " && "
  override def opSVA: String = " and "
}
case class or_prop(var lchild:cha_pro, var rchild:cha_pro) extends bin_op_pro
{
  override def opPSL: String = " || "
  override def opSVA: String = " or "
}
case class until_prop(var lchild:cha_pro, var rchild:cha_pro) extends bin_op_pro
{
  override def opPSL: String = " U "
  override def opSVA: String = " s_until "
}
case class impl_prop(var lchild:cha_pro, var rchild:cha_pro) extends bin_op_pro
{
  override def opPSL: String = " -> "
  override def opSVA: String = " implies "
}
case class glo_prop(var child:cha_pro) extends un_op_pro
{
  override def opPSLL: String = "G "
  override def opPSLR: String = ""

  override def opSVAL: String = "always "
  override def opSVAR: String = ""
}
case class fina_prop(var child:cha_pro) extends un_op_pro
{
  override def opPSLL: String = "F "
  override def opPSLR: String = ""

  override def opSVAL: String = "s_eventually "
  override def opSVAR: String = ""
}

case class over_impl_prop(var lchild:cha_seq, var rchild:cha_pro) extends bin_op_node[cha_seq, cha_pro] with cha_pro
{
  override def opPSL: String = " []-> "
  override def opSVA: String = " |-> "
}

case class noover_impl_prop(var lchild:cha_seq, var rchild:cha_pro) extends bin_op_node[cha_seq, cha_pro] with cha_pro
{
  override def opPSL: String = " []=> "
  override def opSVA: String = " |=> "
}

case class prompt_prop(var child:cha_seq) extends un_op_node[cha_seq] with cha_pro
{
  override def opPSLL: String = ""
  override def opPSLR: String = ""

  override def opSVAL: String = ""
  override def opSVAR: String = ""
  override def toPSL(rename2p: Map[Target,String]): String = 
  {
    "(" + "{" + child.asInstanceOf[chaElementAnno].toPSL(rename2p) + "}" +")" 
  }
}

trait targetAnno extends cha_node
{
  val target: Target
}

case class ResetAnno(target:Target) extends targetAnno
case class EnableAnno(target:Target) extends targetAnno
case class ClockAnno(target:Target) extends targetAnno
case class ModAnno(target:Target) extends targetAnno
case class InitialAssertion() extends cha_node

class cha_tree(o:Object) extends JavaTokenParsers {
  def prop: Parser[cha_pro] = 
  (
      prop6 ^^{case p => p}
  )
  def prop6: Parser[cha_pro] =
  (
      "G"~>prop6                    ^^ {case p:cha_pro =>  glo_prop(p)}
    | "F"~>prop6                    ^^ {case p:cha_pro =>  fina_prop(p)}
    | prop5                         ^^ {case p:cha_pro =>  p}
  )
  def prop5: Parser[cha_pro] =
  (
    seq~"|->"~prop6                 ^^ {case ~(~(p1,o),p2) =>  over_impl_prop(p1,p2)}
    | seq~"|=>"~prop6                 ^^ {case ~(~(p1,o),p2) =>  noover_impl_prop(p1,p2)}
    | prop4                         ^^ {case p:cha_pro =>  p}
  )
  def prop4: Parser[cha_pro] =
  (
      prop3~"U"~prop6               ^^ {case ~(~(p1,o),p2) =>  until_prop(p1,p2)}
    | prop3~"->"~prop6              ^^ {case ~(~(p1,o),p2) =>  impl_prop(p1,p2)}
    | prop3                         ^^ {case p:cha_pro =>  p}
  )
  def prop3: Parser[cha_pro] =
  (
    prop2~"||"~prop6                ^^ {case ~(~(p1,o),p2) => or_prop(p1,p2)}
    | prop2                         ^^ {case p:cha_pro =>  p}
                                      
  )
  def prop2: Parser[cha_pro] =
  (
    prop1~"&&"~prop6                ^^ {case ~(~(p1,o),p2) => and_prop(p1,p2)}
    | prop1                         ^^ {case p:cha_pro =>  p}
  )
  def prop1: Parser[cha_pro] = 
  (
      seq       ^^ {case s:cha_seq => prompt_prop(s)}
    |  "!"~>prop1 ^^ {case p:cha_pro => not_prop(p)}  
    | "X"~>prop1 ^^ {case p:cha_pro => next_prop(p)}
    | "("~>prop<~")" ^^{case p:cha_pro => p}
    |  "!"~>prop ^^ {case p:cha_pro => not_prop(p)}  
    |  "X"~>prop ^^ {case p:cha_pro => next_prop(p)}
    // | "G"~>prop ^^ {case p:cha_pro => glo_prop(p)}
  )

  // Use seq1,seq2 to maintain the precedence: ## > && > or
  // Unary sequence operator repetition has highest priority (in repe) 
  def seq: Parser[cha_seq] =
  (
    opt(time_delay)~seq3         ^^ {case ~(Some(t),s) =>  time_delay_seq(t.lowerBound,t.upperBound,constantTrue(),s)
                                     case ~(None,s) => s
                                            }
  )
  def seq3: Parser[cha_seq] = 
  (
    seq2~opt("|"~seq)              ^^ {case ~(s1,Some(~(o,s2))) =>  or_seq(s1,s2)
                                      case ~(s,None) =>  s
                                      case x => new atom_prop_node(false.asBool)}
  )
  def seq2: Parser[cha_seq] = 
  (
    seq1~opt("&"~seq2)             ^^ {case ~(s1,Some(~(o,s2))) =>  and_seq(s1,s2)
                                      case ~(s,None) => s
                                      case x => new atom_prop_node(false.asBool)}
    // seq1 ^^ {case p => println(s"seq: $p"); p}
  )
  def seq1: Parser[cha_seq] = 
  (
    repe~opt(time_delay~seq1)       ^^ {case ~(r,Some(~(t,s))) =>  time_delay_seq(t.lowerBound,t.upperBound,r,s)
                                      case ~(r,None) => r
                                      case x => new atom_prop_node(false.asBool)}
  )
    
  //##m, ##[m:n], ##[*], ##[+]
  def time_delay: Parser[time_delay_op] = 
  (
    // "##"~(wholeNumber | "["~(wholeNumber~":"~wholeNumber | wholeNumber~":"~"$" | "*" | "+")~"]") 
      "##"~>tuple2                  ^^ {t => time_delay_op(t._1,t._2)}
    | "##"~"["~>tuple2<~"]"         ^^ {t => time_delay_op(t._1,t._2)}
  )
  def repe: Parser[cha_seq] = 
  (
      atom_prop~opt(repe_range)     ^^ {case ~(ap,Some(r)) => repe_seq(r.lowerBound,r.upperBound,ap)
                                      case ~(ap,None)    => ap}
    | "!"~>atom_prop~opt(repe_range)^^ {case ~(ap,Some(r)) => repe_seq(r.lowerBound,r.upperBound,!ap)
                                      case ~(ap,None)    => !ap}                                
    | "("~>seq~")"~opt(repe_range)  ^^ {case ~(~(seq,")"),Some(r)) => repe_seq(r.lowerBound,r.upperBound,seq)
                                      case ~(~(seq,")"),None) => seq
                                      case _ => new atom_prop_node(false.asBool)}
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
                                          //Notice: no exception handling
                                          val variable = o.getClass.getMethod(x).invoke(o)
                                          atom_prop_node(variable.asInstanceOf[Bool])
                                        }}
    | """[a-zA-Z_&&[^XFG]]""".r  ^^ {x => 
                                        {
                                          //Notice: no exception handling
                                          val variable = o.getClass.getMethod(x).invoke(o)
                                          atom_prop_node(variable.asInstanceOf[Bool])
                                        }}
  )  
}

trait chaStmt
case object chaAssertStmt extends chaStmt
case object chaAssumeStmt extends chaStmt
case object chaCoverStmt extends chaStmt

object chaAnno
{
  def chaAssert(o:Object, s:String, isInitial:Boolean = false) =
  {
    val res = o.asInstanceOf[Module].reset
    val en = !res.asBool
    dontTouch(en)
    val clo = o.asInstanceOf[Module].clock
    dontTouch(clo)
    val mod = o.asInstanceOf[Module]
    val chaTree = new cha_tree(o)
    val syntaxTree = chaTree.parseAll(chaTree.prop6, s)
    // println(s"$res, $syntaxTree")
    val chaSeq = syntaxTree.get.treeSerialize()

    chaSeq.foreach{
      case a: atom_prop_node => dontTouch(a.signal) 
      case b => 
    }
    annotate(new ChiselAnnotation {
      // Conversion to FIRRTL Annotation 
      override def toFirrtl: Annotation = 
      {
        val chaanotation : Seq[Seq[cha_node]] = chaSeq map {
          case atom_prop_node(ap) => Seq(atom_prop_anno(ap.toTarget))
          case otherOp: chaElementAnno => Seq(otherOp)
        } 
        // println(s"chaAnnotation: ${chaanotation.toSeq}")
        val commonAttached = Seq(Seq(ResetAnno(res.toTarget)),Seq(ClockAnno(clo.toTarget)), Seq(ModAnno(mod.toTarget)), Seq(EnableAnno(en.toTarget)))
        val attachedAnnos = 
          if(!isInitial) commonAttached
          else commonAttached :+ Seq(InitialAssertion())
        val ret = new chaAssertAnno(chaanotation ++ attachedAnnos)
        println(s"chaAssertStmt: ${chaAnno.CHAAnno2PSL(ret)._1}")
        ret
        // new chaAnno(chaanotation:+Seq(ClockAnno(clo.toTarget)):+Seq(ModAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
      }
    })
  }

  def chaAssume(o:Object, s:String) =     
  {
    val res = o.asInstanceOf[Module].reset
    val en = !res.asBool
    dontTouch(en)
    val clo = o.asInstanceOf[Module].clock
    val mod = o.asInstanceOf[Module]
    val chaTree = new cha_tree(o)
    val syntaxTree = chaTree.parseAll(chaTree.prop6, s)
    // println(s"$res, $syntaxTree")
    val chaSeq = syntaxTree.get.treeSerialize()

    chaSeq.foreach{
      case a: atom_prop_node => dontTouch(a.signal) 
      case b => 
    }
    annotate(new ChiselAnnotation {
      // Conversion to FIRRTL Annotation 
      override def toFirrtl: Annotation = 
      {
        val chaanotation : Seq[Seq[cha_node]] = chaSeq map {
          case atom_prop_node(ap) => Seq(atom_prop_anno(ap.toTarget))
          case otherOp: chaElementAnno => Seq(otherOp)
        } 
        val ret = new chaAssumeAnno(chaanotation:+Seq(ResetAnno(res.toTarget)):+Seq(ClockAnno(clo.toTarget)):+Seq(ModAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
        println(s"chaAssumeStmt: ${chaAnno.CHAAnno2PSL(ret)._1}")
        ret
      }
    })
  }

  def chaCover(o:Object, s:String, isInitial:Boolean = false) =
  {
    val res = o.asInstanceOf[Module].reset
    val en = !res.asBool
    dontTouch(en)
    val clo = o.asInstanceOf[Module].clock
    dontTouch(clo)
    val mod = o.asInstanceOf[Module]
    val chaTree = new cha_tree(o)
    val syntaxTree = chaTree.parseAll(chaTree.prop6, s)
    // println(s"$res, $syntaxTree")
    val chaSeq = syntaxTree.get.treeSerialize()

    chaSeq.foreach{
      case a: atom_prop_node => dontTouch(a.signal) 
      case b => 
    }
    annotate(new ChiselAnnotation {
      // Conversion to FIRRTL Annotation 
      override def toFirrtl: Annotation = 
      {
        val chaanotation : Seq[Seq[cha_node]] = chaSeq map {
          case atom_prop_node(ap) => Seq(atom_prop_anno(ap.toTarget))
          case otherOp: chaElementAnno => Seq(otherOp)
        } 
        // println(s"chaAnnotation: ${chaanotation.toSeq}")
        val commonAttached = Seq(Seq(ResetAnno(res.toTarget)),Seq(ClockAnno(clo.toTarget)), Seq(ModAnno(mod.toTarget)), Seq(EnableAnno(en.toTarget)))
        // cover should not be initial assertion
        val attachedAnnos = commonAttached
        // if(!isInitial) commonAttached
        // else commonAttached :+ Seq(InitialAssertion())
        val ret = new chaCoverAnno(chaanotation ++ attachedAnnos)
        println(s"chaCoverStmt: ${chaAnno.CHAAnno2PSL(ret)._1}")
        ret
        // new chaAnno(chaanotation:+Seq(ClockAnno(clo.toTarget)):+Seq(ModAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
      }
    })
  }

  
  def generateMap2p(seq:Seq[cha_node]) : Map[Target,String] =
  {
    var i:Int = 0
    val temp = seq.collect{
      case t: targetAnno => t.target
      case atom_prop_anno(target) => target
    }.distinct
    temp.map(a => a->("p" + {i+=1; i})).toMap
  }

  def CHAAnno2PSL(s: chaAnno): Tuple4[String, Map[String,Target], Target, chaStmt] = 
  {
    val elementCHA = s.toElementSeq().toSeq
    // println(s"elementCHA: $elementCHA")
    val resetAn = elementCHA.filter(_.isInstanceOf[ResetAnno])
    // assert(resetAn.size == 1,"only allow one reset signal")
    val isInitial = elementCHA.exists(_.isInstanceOf[InitialAssertion])
    
    val remainCHA = elementCHA.filter{x => (!x.isInstanceOf[targetAnno] && !x.isInstanceOf[InitialAssertion])}
    // println(s"remainCHA: $remainCHA")
    val target2p = chaAnno.generateMap2p(remainCHA)
    val p2target = target2p.toSeq.map{case Tuple2(k,v) => Tuple2(v,k)}.toMap

    val deSeri = remainCHA(0).treeDeSerialize(remainCHA.tail)
    // println(s"deserialization: ${deSeri}")

    // distinguish assert with assume, assert statement need to be negated
    val isAssert = s.isInstanceOf[chaAssertAnno]
    val isCover = s.isInstanceOf[chaCoverAnno]
    val neg = if (isAssert) "! " else ""
    val initial =  if(isCover) "F " else if(!isInitial) "G " else "" 
    val psl = neg + initial + "(" + deSeri._1.asInstanceOf[chaElementAnno].toPSL(target2p) + ") "
    // val psl = neg  + deSeri._1.asInstanceOf[chaElementAnno].toPSL(target2p)
    //val psl = "!" + chaAnno.toPSL(syntaxTree,target2p)
    // println(s"psl: $psl")
    val stmt = if (isAssert) chaAssertStmt else if(isCover) chaCoverStmt else chaAssumeStmt
    // println(s"stmt: $stmt, psl: $psl")
    (psl,p2target,resetAn(0).asInstanceOf[ResetAnno].target,stmt)
  }
}



case class chaAssumeAnno(ttargets: Seq[Seq[cha_node]]) extends chaAnno
case class chaAssertAnno(ttargets: Seq[Seq[cha_node]]) extends chaAnno
case class chaCoverAnno(ttargets: Seq[Seq[cha_node]]) extends chaAnno

trait chaAnno extends MultiTargetAnnotation{
  //ttargets.filter(_.isInstanceOf[atom_prop_anno])
  val ttargets: Seq[Seq[cha_node]]
  def copy(ts: Seq[Seq[cha_node]]) = {
    this match {
      case chaAssertAnno(ttargets) => chaAssertAnno(ts)
      case chaAssumeAnno(ttargets) => chaAssumeAnno(ts)
      case chaCoverAnno(ttargets) => chaCoverAnno(ts)
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
        case ClockAnno(target) => {renames(target).map{ClockAnno(_)}}
        case EnableAnno(target) => renames(target).map{EnableAnno(_)}
        case ModAnno(target) => renames(target).map{ModAnno(_)}
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
  
  def toElementSeq(): Seq[cha_node] = ttargets.flatMap(_.slice(0,1))
}



case class target2ExprAnno(getMap: Map[Target,Expression]) extends NoTargetAnnotation

