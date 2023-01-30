// package chiseltest.formal

// import chisel3._
// import chiseltest.formal.past._
// import scala.util.control.Breaks._
// import scala.language.postfixOps
// import scala.collection.mutable

// object TSequence
// {
//   def apply(input: Seq[svaElement]) = {
//     //val out = input.foldLeft(true.B)((p,q)=>parseTSequence(p,q))  
//     val initInput = initTSeq(input)
//     println(input.toString())
//     println("---------")
//     println(initInput.toString())
//     val out = parseTSequence(initInput)

//     WireInit(out)
//   }


//   private def initTSeq(input:Seq[svaElement]):Seq[svaElement] = 
//   {
//     import scala.collection.mutable.Seq
//     var retSeq = scala.collection.mutable.Seq[svaElement]()
//     for(i <- 0 until input.size)
//     {
//       if(retSeq.isEmpty)
//       {
//         retSeq :+= input(i)
//       }
//       else
//       {
//         input(i) match
//         {
//           case AtmProp(ap1) =>
//           {
//             retSeq(retSeq.size-1) match 
//             {
//               case AtmProp(ap2) => retSeq(retSeq.size-1) = AtmProp(ap1 & ap2)
//               case _ => retSeq :+= AtmProp(ap1) 
//             }
//           }
//           case TimeOp(lc1, hc1) =>
//           {
//             retSeq(retSeq.size-1) match 
//             {
//               case TimeOp(lc2, hc2) => retSeq(retSeq.size-1) = TimeOp(lc1+lc2, hc1+hc2)
//               case _ => retSeq :+= TimeOp(lc1, hc1)
//             }
//           }
//           case other => retSeq :+= other
//         }
//       }  
//     }
//     retSeq.toSeq
//   }
//   private def parseTSequenceNoImply(restSeq:Seq[svaElement]): (Int,Bool) =
//   {
//     if(restSeq.size == 0)
//     {
//       (0,true.B)
//     }
//     else if(restSeq.size == 1 && restSeq(0).isInstanceOf[AtmProp])
//     {
//       (1,restSeq(0).asInstanceOf[AtmProp].signal)
//     }
//     // else if(restSeq.size == 2 && restSeq(0).isInstanceOf[AtmProp] && restSeq(1).isInstanceOf[RepetOp])
//     // {
//     //   println("this!!!")
//     //   val ap:AtmProp = restSeq(0).asInstanceOf[AtmProp]
//     //   val repOp:RepetOp = restSeq(1).asInstanceOf[RepetOp]
//     //   val lowerBounds = repOp.lowerBounds
//     //   val upperBounds = repOp.upperBounds
//     //   val beforeLower = (0 until lowerBounds).foldLeft(past(ap.signal,lowerBounds))((p, _) =>{ p && past(p) })
//     //   (upperBounds, ( 0 until upperBounds-lowerBounds).foldLeft(ap.signal)((p, _) =>{ p || past(p) }) && beforeLower )  
//     //   //(1,restSeq(0).asInstanceOf[AtmProp].signal)
//     // }
//     else
//     {
//       if(restSeq.size == 2 && restSeq(0).isInstanceOf[AtmProp] && restSeq(1).isInstanceOf[RepetOp])
//       {
//          val ap:AtmProp = restSeq(0).asInstanceOf[AtmProp]
//          val repOp:RepetOp = restSeq(1).asInstanceOf[RepetOp]
//          val lowerBounds = repOp.lowerBounds
//          val beforeLower = (0 until lowerBounds-1).foldLeft(ap.signal)((p, _) =>{ p && past(p) })
//          (lowerBounds,beforeLower)
//       }
//       else
//       {
//         val firstTO:Int = restSeq.indexWhere(_.isInstanceOf[TimeOp])
//         //println(firstTO)
//         //println(restSeq.toString())
//         if(firstTO > 0 && restSeq(firstTO-1).isInstanceOf[RepetOp])
//         {
//           val ap:AtmProp = restSeq(firstTO-2).asInstanceOf[AtmProp]
//           val repOp:RepetOp = restSeq(firstTO-1).asInstanceOf[RepetOp]
//           val lowerBounds = repOp.lowerBounds
//           val upperBounds = repOp.upperBounds

//           val ret2 = parseTSequenceNoImply(restSeq.slice(firstTO,restSeq.size))
//           val beforeLower = past((0 until lowerBounds-1).foldLeft(ap.signal)((p, _) =>{ p && past(p) }), (upperBounds-lowerBounds+ret2._1))
//           var temp:mutable.Seq[Bool] = mutable.Seq(past(ap.signal,ret2._1))
//           for(i <- 1 until upperBounds-lowerBounds)
//           {
//             temp :+= past(temp(i-1))
//           }
//           temp = temp.reverse
//           for(i <- 1 until upperBounds-lowerBounds)
//           {
//             temp(i) = temp(i) && temp(i-1)
//           }
//           temp +:= true.B
//           var temp2:mutable.Seq[Bool] = mutable.Seq(ret2._2)
//           for(i <- 1 until upperBounds-lowerBounds+1)
//           {
//             temp2 :+= past(temp(i-1))
//           }
//           temp2 = temp2.reverse
//           var partialExpr = temp(0) && temp2(0)
//           for(i <- 1 until temp.size)
//           {
//             partialExpr = partialExpr || (temp(i) && temp2(i))
//           }
//           (ret2._1+upperBounds,partialExpr && beforeLower)
//         }
//         else
//         {
//           val timeOp = restSeq(firstTO).asInstanceOf[TimeOp]
//           val ret1 = parseTSequenceNoImply(restSeq.slice(0,firstTO))
//           val ret2 = parseTSequenceNoImply(restSeq.slice(firstTO+1,restSeq.size))
//           val temp = ( 0 until timeOp.upperBounds-timeOp.lowerBounds).foldLeft(ret2._2)((p, _) =>{ p || past(p) })
//           val total = past(ret1._2,ret2._1+timeOp.upperBounds) && temp
//           (ret1._1+timeOp.upperBounds+ret2._1,total)
//         }
//       }
//     }
//   }

//   private def parseTSequence(restSeq:Seq[svaElement]): (Bool) =
//   {
//     var lastImply:Int = restSeq.lastIndexOf(ImplicationOp())
//     var ret = (0,true.B)
//     if(lastImply < 0)
//     {
//       ret = parseTSequenceNoImply(restSeq)  
//     }
//     else
//     {
//       val deletedSeq = restSeq.slice(0,lastImply).filter(!_.isInstanceOf[ImplicationOp]) ++ restSeq.slice(lastImply,restSeq.size)
//       val deletedSeq_ = initTSeq(deletedSeq)
//       //println("-------")
//       //println(deletedSeq.toString())
//       lastImply = deletedSeq.lastIndexOf(ImplicationOp())
//       val ret1 = parseTSequenceNoImply(deletedSeq.slice(0,lastImply))
//       val ret2 = parseTSequenceNoImply(deletedSeq.slice(lastImply+1,deletedSeq.size))
//       ret = (ret1._1+ret2._1-1,!past(ret1._2,ret2._1-1) || ret2._2)
//     }
//     val delay:UInt = (ret._1-1).asUInt
//     println(s"delay is $delay")
//     /*val cntReg = RegInit(0.U(delay.getWidth))
//     when(cntReg < delay){
//       cntReg := cntReg + 1.U
//     }
//     when(cntReg === delay){
//       println("can reach delay")
//     }
//     || cntReg < delay*/
//     ret._2 
//   }
// }