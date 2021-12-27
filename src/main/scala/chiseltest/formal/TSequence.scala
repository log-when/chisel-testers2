package chiseltest.formal

import chisel3._
import chiseltest.formal.past
import scala.util.control.Breaks._
import scala.language.postfixOps

object TSequence
{
  def apply(input: Seq[TSeqElement]) = {
    //val out = input.foldLeft(true.B)((p,q)=>parseTSequence(p,q))  
    val initInput = initTSeq(input)
    println(input.toString())
    println("---------")
    println(initInput.toString())
    val out = parseTSequence(initInput)

    WireInit(out)
  }


  private def initTSeq(input:Seq[TSeqElement]):Seq[TSeqElement] = 
  {
    import scala.collection.mutable.Seq
    var retSeq = scala.collection.mutable.Seq[TSeqElement]()
    for(i <- 0 until input.size)
    {
      if(retSeq.isEmpty)
      {
        retSeq :+= input(i)
      }
      else
      {
        input(i) match
        {
          case AtmProp(ap1) =>
          {
            retSeq(retSeq.size-1) match 
            {
              case AtmProp(ap2) => retSeq(retSeq.size-1) = AtmProp(ap1 & ap2)
              case _ => retSeq :+= AtmProp(ap1) 
            }
          }
          case TimeOp(lc1, hc1) =>
          {
            retSeq(retSeq.size-1) match 
            {
              case TimeOp(lc2, hc2) => retSeq(retSeq.size-1) = TimeOp(lc1+lc2, hc1+hc2)
              case _ => retSeq :+= TimeOp(lc1, hc1)
            }
          }
          case other => retSeq :+= other
        }
      }  
    }
    retSeq.toSeq
  }
  private def parseTSequenceNoImply(restSeq:Seq[TSeqElement]): (Int,Bool) =
  {
    if(restSeq.size == 0)
    {
      (0,true.B)
    }
    else if(restSeq.size == 1 && restSeq(0).isInstanceOf[AtmProp])
    {
      (1,restSeq(0).asInstanceOf[AtmProp].signal)
    }
    else
    {
      val firstTO:Int = restSeq.indexWhere(_.isInstanceOf[TimeOp])
      //println(firstTO)
      //println(restSeq.toString())
      val timeOp = restSeq(firstTO).asInstanceOf[TimeOp]
      val ret1 = parseTSequenceNoImply(restSeq.slice(0,firstTO))
      val ret2 = parseTSequenceNoImply(restSeq.slice(firstTO+1,restSeq.size))
      val temp = ( 0 until timeOp.upperCycles-timeOp.lowerCycles+1).foldLeft(ret2._2)((p, _) =>{ p || past(p) })
      if(ret1._1 == 0)
      {
        //val total = past(ret1._2,ret2._1+timeOp.upperCycles-1) && temp
        (ret1._1+timeOp.upperCycles+ret2._1,temp)
      }
      else
      {
        val total = past(ret1._2,ret2._1+timeOp.upperCycles) && temp
        (ret1._1+timeOp.upperCycles+ret2._1,total)
      }
    }
  }

  private def parseTSequence(restSeq:Seq[TSeqElement]): (Bool) =
  {
    var lastImply:Int = restSeq.lastIndexOf(Implication())
    var ret = (0,true.B)
    if(lastImply < 0)
    {
      ret = parseTSequenceNoImply(restSeq)  
    }
    else
    {
      val deletedSeq = restSeq.slice(0,lastImply).filter(!_.isInstanceOf[Implication]) ++ restSeq.slice(lastImply,restSeq.size)
      val deletedSeq_ = initTSeq(deletedSeq)
      //println("-------")
      //println(deletedSeq.toString())
      lastImply = deletedSeq.lastIndexOf(Implication())
      val ret1 = parseTSequenceNoImply(deletedSeq.slice(0,lastImply))
      val ret2 = parseTSequenceNoImply(deletedSeq.slice(lastImply+1,deletedSeq.size))
      if(ret1._1 == 0)
      {
        ret = ret2
      }
      else
      {
        ret = (ret1._1+ret2._1-1,!past(ret1._2,ret2._1-1) || ret2._2)
      }
    }
    /*val delay:UInt = (ret._1-1).asUInt
    println(s"delay is $delay")
    val cntReg = RegInit(0.U(delay.getWidth))
    when(cntReg < delay){
      cntReg := cntReg + 1.U
    }
    when(cntReg === delay){
      println("can reach delay")
    }
    || cntReg < delay*/
    ret._2 
  }
}