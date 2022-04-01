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
  if( seq.isEmpty ){
      None
    }
    else{
      seq(0) match {
        case NotAnno() => SVANode(NotAnno,toSVATree(seq.slice(1,seq.size)),None)
        case LeftbraketAnno() => {
          var n = 1
          var bre = false
          var left = -1
          for(i <- 1 until seq.size)
          {
            if(n != 0)
            {
              seq(i) match 
              {
                case LeftbraketAnno => n = n + 1
                case RightbraketAnno => n = n - 1
              }
              if(n = 0)
                left = i
            }
          }
          toSVATree(seq.slice(1,left))
        }
        case RightbraketAnno() => {println("mistake!") None}
        case _ => {
          val firstImp = seq.indexWhere(_.isInstanceOf[ImplicationAnno])
          if(firstImp = -1)
          {
            val firstTime = seq.indexWhere(_.isInstanceOf[TimeOpAnno])
            SVANode(seq(firstTime), toSVATree(seq.slice(0,firstTime-1)), toSVATree(seq.slice(firstTime+1,seq.size)))
          }
          else
          {
            SVANode(ImplicationAnno(), toSVATree(seq.slice(0,firstImp-1)), toSVATree(seq.slice(firstImp+1,seq.size)))
          }

        }
      }
    }

    
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
        case TimeOpAnno(lowerCycles, upperCycles) => Seq(TimeOpAnno(lowerCycles, upperCycles))
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