package chiseltest.formal

import chisel3.experimental.{ChiselAnnotation,annotate}
import firrtl.annotations.{Annotation, ReferenceTarget, SingleTargetAnnotation, Target,MultiTargetAnnotation}
import firrtl.options.StageUtils
import firrtl.RenameMap
import org.json4s.JValue
import firrtl._

import scala.collection.Traversable

case class APAnno(val target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
  def duplicate(n: ReferenceTarget): Annotation = this.copy(target = n)
}



trait TSeqElementAnno

case class AtmPropAnno(target:Target) extends TSeqElementAnno

case class TimeOpAnno(lowerCycles: Int, upperCycles: Int) extends TSeqElementAnno

case class LeftbraketAnno() extends TSeqElementAnno

case class RightbraketAnno() extends TSeqElementAnno

case class NotAnno() extends TSeqElementAnno

case class ImplicationAnno() extends TSeqElementAnno

case class FinallAnno() extends TSeqElementAnno

case class GlobalAnno() extends TSeqElementAnno

case class NextAnno() extends TSeqElementAnno

case class RepetAnno() extends TSeqElementAnno




case class SVANode(ele:TSeqElementAnno, left:SVANode, right: SVANode)
/*case class SAnno(targets: Seq[Target]) extends MultiTargetAnnotation
{
  override def duplicate(a: Seq[Seq[Target]]) = this.copy(targets = a)
  //override def update(renames: RenameMap): Seq[Annotation] = Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))

}*/
//case class SomeAnno(t0: Target, t1: Target, yourOwnMetaData: XXX) extends MultiTargetAnnotaion

object SVAAnno
{
  def toSVATree(seq:Seq[TSeqElementAnno]): SVANode = {
    if( seq.isEmpty )
    {
      null
    }
    else if(seq.size == 1)
    {
      seq(0) match {
        case AtmPropAnno(target) => SVANode(AtmPropAnno(target), null, null)
        case _ => null
      }
    }
    else{
      seq(0) match {
        case NotAnno() => SVANode(NotAnno(),toSVATree(seq.slice(1,seq.size)),null)
        case FinallAnno() => SVANode(FinallAnno(),toSVATree(seq.slice(1,seq.size)),null)
        case LeftbraketAnno() => {
          var n = 1
          var bre = false
          var left = -1
          for(i <- 1 until seq.size)
          {
            if(n != 0)
            {
              if(seq(i).isInstanceOf[LeftbraketAnno])
                n = n + 1
              else if(seq(i).isInstanceOf[RightbraketAnno])
                n = n - 1
              if(n == 0)
                left = i
            }
          }
          println(s"right: $left")
          toSVATree(seq.slice(1,left))
        }
        case RightbraketAnno() => {println("mistake!"); null}
        // case AtmPropAnno(target) => SVANode(AtmPropAnno(target), null, null)
        case _ => {
          //pay attention
          //there exists some bug to be fixed!!!
          //pay attention
          val firstImp = seq.indexWhere(_.isInstanceOf[ImplicationAnno])
          if(firstImp == -1)
          {
            val lastRepet = seq.lastIndexWhere(_.isInstanceOf[RepetAnno])
            if(lastRepet == -1)
            {
              val firstTime = seq.indexWhere(_.isInstanceOf[TimeOpAnno])
              println(firstTime)
              if(firstTime < seq.size)
                SVANode(seq(firstTime), toSVATree(seq.slice(0,firstTime)), toSVATree(seq.slice(firstTime+1,seq.size)))
              else
                SVANode(seq(firstTime), toSVATree(seq.slice(0,firstTime)), null)
            }
            else
            {
              SVANode(RepetAnno(), toSVATree(seq.slice(0,lastRepet)), toSVATree(seq.slice(lastRepet+1,seq.size)))
            }
          }
          else
          {
            SVANode(ImplicationAnno(), toSVATree(seq.slice(0,firstImp)), toSVATree(seq.slice(firstImp+1,seq.size)))
          }
        }
      }
    }
  }

  def generateMap2p(seq:Seq[TSeqElementAnno]) : Map[Target,String] =
  {
    var i:Int = 0
    val temp = seq.collect{case AtmPropAnno(target) => target}.distinct
    temp.map(a => a->("p" + {i+=1; i})).toMap
  }

  def toPSL(syntaxTree: SVANode, rename2p: Map[Target,String]) : String = 
  {
    syntaxTree match {
      case SVANode(ImplicationAnno(),left,right) => "{" + toPSL(left,rename2p) + "}" +"[]->" + toPSL(right,rename2p)
      case SVANode(AtmPropAnno(target),null,null) => {rename2p(target)}
      case SVANode(TimeOpAnno(lc,hc),left,right) => {
        if(hc != -1)
          toPSL(left,rename2p) + ";"+ "true[*" + lc + ".." + hc + "]" +";"+ toPSL(right,rename2p)
        else 
          toPSL(left,rename2p) + ";"+ "true[*" + lc + "]" + ";" + "true[*]" +";"+ toPSL(right,rename2p)
      }
      case SVANode(NotAnno(),left,right) => "!(" + toPSL(left,rename2p) +")"
      case SVANode(FinallAnno(),left,right) => "F " + toPSL(left,rename2p)
      case SVANode(RepetAnno(),left,right) => "(" + toPSL(left,rename2p) + ")" + "[*]"
      
      case null => ""
      case a => {println("unsupported operator"); ""}
    }
  }


  def SVAAnno2PSL(s: SVAAnno) : Tuple2[String, Map[String,Target]] = 
  {
    val elementSVA = s.toElementSeq().toSeq
    println(s"elementSVA: $elementSVA")
    val target2p = SVAAnno.generateMap2p(elementSVA)
    val p2target = target2p.toSeq.map{case Tuple2(k,v) => Tuple2(v,k)}.toMap
    println(p2target)
    val syntaxTree = SVAAnno.toSVATree(elementSVA)
    println(SVAAnno.toSVATree(elementSVA))
    val psl = "!" + SVAAnno.toPSL(syntaxTree,target2p)
    (psl,p2target)
  }
}

case class SVAAnno(ttargets: Seq[Seq[TSeqElementAnno]]) extends MultiTargetAnnotation{
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
        case a => Seq(a)
      }
  )))
  }

  /*sameModule should be after flat, since we hope ttargets is like Seq(Seq(TSeqElementAnno), Seq(TSeqElementAnno), ...)  */
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
  
  def toElementSeq(): Seq[TSeqElementAnno] = ttargets.flatMap(_.slice(0,1))
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
        case Seq(TimeOpAnno(lowerCycles, upperCycles)) => 
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
        case TimeOpAnno(lowerCycles, upperCycles) => 
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
      case t:TimeOpAnno =>
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
      case TimeOpAnno(lc,up) =>TimeOpAnno(lc,up)
    }*/
    Seq(this.copy(ttemp))
  }*/
}

/*case class SVAAnno(ts:TSeq)
{

}*/