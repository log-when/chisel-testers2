diff --git a/build.sbt b/build.sbt
index 381a9ad..e7a5787 100644
--- a/build.sbt
+++ b/build.sbt
@@ -3,7 +3,7 @@
 organization := "edu.berkeley.cs"
 name := "chiseltest"
 
-version := "0.6-SNAPSHOT"
+version := "0.7-SNAPSHOT"
 
 scalaVersion := "2.13.10"
 
@@ -74,6 +74,7 @@ libraryDependencies ++= Seq(
   "com.lihaoyi" %% "utest" % "0.8.1",
   "net.java.dev.jna" % "jna" % "5.13.0",
   "org.scala-lang" % "scala-reflect" % scalaVersion.value,
+  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
   compilerPlugin(("edu.berkeley.cs" % "chisel3-plugin" % defaultVersions("chisel3")).cross(CrossVersion.full))
 ) ++ {
   CrossVersion.partialVersion(scalaVersion.value) match {
diff --git a/build.sc b/build.sc
index 115fa4d..bb0d418 100644
--- a/build.sc
+++ b/build.sc
@@ -58,7 +58,7 @@ class chiseltestCrossModule(val crossScalaVersion: String)
     super.javacOptions() ++ Seq("-source", "1.8", "-target", "1.8")
   }
 
-  override def moduleDeps = super.moduleDeps ++ chisel3Module ++ treadleModule
+  override def moduleDeps = super.moduleDeps ++ chisel3Module
 
   override def ivyDeps = T {
     Agg(
diff --git a/lib/ROLL.jar b/lib/ROLL.jar
new file mode 100644
index 0000000..59c5c9b
Binary files /dev/null and b/lib/ROLL.jar differ
diff --git a/lib/javabdd-1.0b2.jar b/lib/javabdd-1.0b2.jar
new file mode 100644
index 0000000..d2b98eb
Binary files /dev/null and b/lib/javabdd-1.0b2.jar differ
diff --git a/lib/roll-library-0.0.1-SNAPSHOT.jar b/lib/roll-library-0.0.1-SNAPSHOT.jar
new file mode 100644
index 0000000..d5c534b
Binary files /dev/null and b/lib/roll-library-0.0.1-SNAPSHOT.jar differ
diff --git a/src/main/scala/chiseltest/formal/BDDManager.scala b/src/main/scala/chiseltest/formal/BDDManager.scala
new file mode 100644
index 0000000..1b97fa7
--- /dev/null
+++ b/src/main/scala/chiseltest/formal/BDDManager.scala
@@ -0,0 +1,60 @@
+package chiseltest.formal
+
+import jhoafparser.ast.Atom
+import jhoafparser.ast.AtomLabel
+import jhoafparser.ast.BooleanExpression
+import jhoafparser.ast.BooleanExpression.Type
+import net.sf.javabdd.BDD
+import net.sf.javabdd.BDDFactory
+import net.sf.javabdd.BDDPairing
+
+class BDDManager(numnNodes:Int = 125000, numCache:Int = 100000, numInc: Int = 10000) {
+
+    var LIB:String = "javabdd"
+    var bdd : BDDFactory = BDDFactory.init(LIB,numnNodes,numCache)
+    
+
+    bdd.setMaxIncrease(numInc)
+
+    def zero():BDD = bdd.zero()
+    def one():BDD = bdd.one()
+    //def extVarNum(num:Int):Unit = bdd.extVarNum(num)
+
+    def ithVar(varr:Int):BDD =
+    {
+        assert(bdd != null)
+        bdd.ithVar(varr)
+    }
+    def setVarNum(num:Int):Unit = 
+    {
+        assert(bdd != null)
+        bdd.setVarNum(num)
+    }
+
+    def extVarNum(num:Int):Unit = 
+    {
+        assert(bdd != null)
+        bdd.extVarNum(num)        
+    }
+
+    @throws(classOf[Exception])
+    def boolExpr2Bdd(boolExpr:BooleanExpression[AtomLabel]): BDD =
+    {
+        assert(boolExpr != null)
+        if(boolExpr.isTRUE())
+            bdd.one()
+        else if(boolExpr.isFALSE())
+            bdd.zero()
+        else if(boolExpr.isAtom())
+            bdd.ithVar(boolExpr.getAtom().getAPIndex())
+        else if(boolExpr.isNOT())
+            boolExpr2Bdd(boolExpr.getLeft()).not()
+        else if(boolExpr.isAND())
+            boolExpr2Bdd(boolExpr.getLeft()).and(boolExpr2Bdd(boolExpr.getRight()))
+        else if(boolExpr.isOR())
+            boolExpr2Bdd(boolExpr.getLeft()).or(boolExpr2Bdd(boolExpr.getRight()))
+        else
+            throw new UnsupportedOperationException("Unsupported BooleanExpression structure")
+    }
+
+}
diff --git a/src/main/scala/chiseltest/formal/Formal.scala b/src/main/scala/chiseltest/formal/Formal.scala
index 5a6a022..0f5ebdc 100644
--- a/src/main/scala/chiseltest/formal/Formal.scala
+++ b/src/main/scala/chiseltest/formal/Formal.scala
@@ -10,9 +10,15 @@ import chiseltest.simulator.{Compiler, WriteVcdAnnotation}
 import firrtl.{AnnotationSeq, CircuitState}
 import firrtl.annotations.NoTargetAnnotation
 import firrtl.transforms.formal.DontAssertSubmoduleAssumptionsAnnotation
+import sys.process._
+import java.io._
+import jhoafparser.parser.HOAFParser
 
 sealed trait FormalOp extends NoTargetAnnotation
 case class BoundedCheck(kMax: Int = -1) extends FormalOp
+case class KInductionCheck(kMax: Int = -1) extends FormalOp
+// IC3SA in pono can't generate witness, only work when properties can be proven... 
+case class Ic3SaCheck(kMax: Int = -1) extends FormalOp
 
 /** Specifies how many cycles the circuit should be reset for. */
 case class ResetOption(cycles: Int = 1) extends NoTargetAnnotation {
@@ -56,8 +62,7 @@ private object Formal {
 
     // add reset assumptions
     val withReset = AddResetAssumptionPass.execute(lowFirrtl)
-
-    // execute operations
+    
     val resetLength = AddResetAssumptionPass.getResetLength(withDefaults)
     ops.foreach(executeOp(withReset, resetLength, _))
   }
@@ -78,7 +83,13 @@ private object Formal {
     annos.collect { case a: FormalOp => a }.distinct
   }
   def executeOp(state: CircuitState, resetLength: Int, op: FormalOp): Unit = op match {
+    // add kInduction engine, it only works when taking pono as the backend
+    // since it only changes the command parameters of calling pono, bmc method is still used 
     case BoundedCheck(kMax) =>
       backends.Maltese.bmc(state.circuit, state.annotations, kMax = kMax, resetLength = resetLength)
+    case KInductionCheck(kMax) => 
+      backends.Maltese.bmc(state.circuit, state.annotations, kMax = kMax, resetLength = resetLength, KInductionCheck(kMax))
+    case Ic3SaCheck(kMax) => 
+      backends.Maltese.bmc(state.circuit, state.annotations, kMax = kMax, resetLength = resetLength, Ic3SaCheck(kMax))
   }
 }
diff --git a/src/main/scala/chiseltest/formal/SVAAnno.scala b/src/main/scala/chiseltest/formal/SVAAnno.scala
new file mode 100644
index 0000000..bb5e21c
--- /dev/null
+++ b/src/main/scala/chiseltest/formal/SVAAnno.scala
@@ -0,0 +1,287 @@
+// package chiseltest.formal
+
+// import chisel3.experimental.{ChiselAnnotation,annotate}
+// import firrtl.annotations.{Annotation, ReferenceTarget, SingleTargetAnnotation, Target,MultiTargetAnnotation}
+// import firrtl.options.StageUtils
+// import firrtl.RenameMap
+// import org.json4s.JValue
+// import firrtl._
+
+// import scala.collection.Traversable
+
+// case class APAnno(val target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] {
+//   def duplicate(n: ReferenceTarget): Annotation = this.copy(target = n)
+// }
+
+
+
+// trait TSeqElementAnno
+
+// case class AtmPropAnno(target:Target) extends TSeqElementAnno
+
+// case class TimeOpAnno(lowerCycles: Int, upperCycles: Int) extends TSeqElementAnno
+
+// case class LeftbraketAnno() extends TSeqElementAnno
+
+// case class RightbraketAnno() extends TSeqElementAnno
+
+// case class NotAnno() extends TSeqElementAnno
+
+// case class ImplicationAnno() extends TSeqElementAnno
+
+// case class FinallAnno() extends TSeqElementAnno
+
+// case class GlobalAnno() extends TSeqElementAnno
+
+// case class NextAnno() extends TSeqElementAnno
+
+// case class RepetAnno() extends TSeqElementAnno
+
+
+
+
+// case class SVANode(ele:TSeqElementAnno, left:SVANode, right: SVANode)
+// /*case class SAnno(targets: Seq[Target]) extends MultiTargetAnnotation
+// {
+//   override def duplicate(a: Seq[Seq[Target]]) = this.copy(targets = a)
+//   //override def update(renames: RenameMap): Seq[Annotation] = Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))
+
+// }*/
+// //case class SomeAnno(t0: Target, t1: Target, yourOwnMetaData: XXX) extends MultiTargetAnnotaion
+
+// object SVAAnno
+// {
+//   def toSVATree(seq:Seq[TSeqElementAnno]): SVANode = {
+//     if( seq.isEmpty )
+//     {
+//       null
+//     }
+//     else if(seq.size == 1)
+//     {
+//       seq(0) match {
+//         case AtmPropAnno(target) => SVANode(AtmPropAnno(target), null, null)
+//         case _ => null
+//       }
+//     }
+//     else{
+//       seq(0) match {
+//         case NotAnno() => SVANode(NotAnno(),toSVATree(seq.slice(1,seq.size)),null)
+//         case FinallAnno() => SVANode(FinallAnno(),toSVATree(seq.slice(1,seq.size)),null)
+//         case LeftbraketAnno() => {
+//           var n = 1
+//           var bre = false
+//           var left = -1
+//           for(i <- 1 until seq.size)
+//           {
+//             if(n != 0)
+//             {
+//               if(seq(i).isInstanceOf[LeftbraketAnno])
+//                 n = n + 1
+//               else if(seq(i).isInstanceOf[RightbraketAnno])
+//                 n = n - 1
+//               if(n == 0)
+//                 left = i
+//             }
+//           }
+//           println(s"right: $left")
+//           toSVATree(seq.slice(1,left))
+//         }
+//         case RightbraketAnno() => {println("mistake!"); null}
+//         // case AtmPropAnno(target) => SVANode(AtmPropAnno(target), null, null)
+//         case _ => {
+//           //pay attention
+//           //there exists some bug to be fixed!!!
+//           //pay attention
+//           val firstImp = seq.indexWhere(_.isInstanceOf[ImplicationAnno])
+//           if(firstImp == -1)
+//           {
+//             val lastRepet = seq.lastIndexWhere(_.isInstanceOf[RepetAnno])
+//             if(lastRepet == -1)
+//             {
+//               val firstTime = seq.indexWhere(_.isInstanceOf[TimeOpAnno])
+//               println(firstTime)
+//               if(firstTime < seq.size)
+//                 SVANode(seq(firstTime), toSVATree(seq.slice(0,firstTime)), toSVATree(seq.slice(firstTime+1,seq.size)))
+//               else
+//                 SVANode(seq(firstTime), toSVATree(seq.slice(0,firstTime)), null)
+//             }
+//             else
+//             {
+//               SVANode(RepetAnno(), toSVATree(seq.slice(0,lastRepet)), toSVATree(seq.slice(lastRepet+1,seq.size)))
+//             }
+//           }
+//           else
+//           {
+//             SVANode(ImplicationAnno(), toSVATree(seq.slice(0,firstImp)), toSVATree(seq.slice(firstImp+1,seq.size)))
+//           }
+//         }
+//       }
+//     }
+//   }
+
+//   def generateMap2p(seq:Seq[TSeqElementAnno]) : Map[Target,String] =
+//   {
+//     var i:Int = 0
+//     val temp = seq.collect{case AtmPropAnno(target) => target}.distinct
+//     temp.map(a => a->("p" + {i+=1; i})).toMap
+//   }
+
+//   def toPSL(syntaxTree: SVANode, rename2p: Map[Target,String]) : String = 
+//   {
+//     syntaxTree match {
+//       case SVANode(ImplicationAnno(),left,right) => "{" + toPSL(left,rename2p) + "}" +"[]->" + toPSL(right,rename2p)
+//       case SVANode(AtmPropAnno(target),null,null) => {rename2p(target)}
+//       case SVANode(TimeOpAnno(lc,hc),left,right) => {
+//         if(hc != -1)
+//           toPSL(left,rename2p) + ";"+ "true[*" + lc + ".." + hc + "]" +";"+ toPSL(right,rename2p)
+//         else 
+//           toPSL(left,rename2p) + ";"+ "true[*" + lc + "]" + ";" + "true[*]" +";"+ toPSL(right,rename2p)
+//       }
+//       case SVANode(NotAnno(),left,right) => "!(" + toPSL(left,rename2p) +")"
+//       case SVANode(FinallAnno(),left,right) => "F " + toPSL(left,rename2p)
+//       case SVANode(RepetAnno(),left,right) => "(" + toPSL(left,rename2p) + ")" + "[*]"
+      
+//       case null => ""
+//       case a => {println("unsupported operator"); ""}
+//     }
+//   }
+
+
+//   def SVAAnno2PSL(s: SVAAnno) : Tuple2[String, Map[String,Target]] = 
+//   {
+//     val elementSVA = s.toElementSeq().toSeq
+//     println(s"elementSVA: $elementSVA")
+//     val target2p = SVAAnno.generateMap2p(elementSVA)
+//     val p2target = target2p.toSeq.map{case Tuple2(k,v) => Tuple2(v,k)}.toMap
+//     println(p2target)
+//     val syntaxTree = SVAAnno.toSVATree(elementSVA)
+//     println(SVAAnno.toSVATree(elementSVA))
+//     val psl = "!" + SVAAnno.toPSL(syntaxTree,target2p)
+//     (psl,p2target)
+//   }
+// }
+
+// case class SVAAnno(ttargets: Seq[Seq[TSeqElementAnno]]) extends MultiTargetAnnotation{
+//   /*println(ttargets.toSeq.toString)
+//   println(ttargets.map(Seq(_)).toSeq.toString)*/
+//   //ttargets.filter(_.isInstanceOf[AtmPropAnno])
+//   override val targets: Seq[Seq[Target]] = ttargets.filter(_.isInstanceOf[AtmPropAnno]).map(t => Seq(t.asInstanceOf[AtmPropAnno].target))
+//   //override duplication, but don't use it!
+//   //override def update(renames: RenameMap): Seq[Annotation] = Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))
+//   //def duplicate(n: T): Annotation
+//   override def duplicate(n: Seq[Seq[Target]]): Annotation =  
+//   { 
+//     val tt:Seq[Target] = n.flatten 
+//     this.copy(Seq(Seq(AtmPropAnno(tt(0))))) 
+//   }
+  
+//   //Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))
+//   override def update(renames: RenameMap) :Seq[Annotation]= 
+//   {
+//     Seq(this.copy(ttargets.map(
+//       ts => ts.flatMap
+//       {
+//         case AtmPropAnno(target) => renames(target).map{AtmPropAnno(_)}
+//         case a => Seq(a)
+//       }
+//   )))
+//   }
+
+//   /*sameModule should be after flat, since we hope ttargets is like Seq(Seq(TSeqElementAnno), Seq(TSeqElementAnno), ...)  */
+//   def sameModule() : Boolean = 
+//   {
+//     val temp = this.ttargets.flatten.collect{case a:AtmPropAnno => a.target.toTarget.moduleOpt}.distinct
+//     //println(temp)
+//     temp.size <= 1
+//   }
+
+//   private def crossJoin[T](list: Seq[Seq[T]]): Seq[Seq[T]] =
+//     list match {
+//       case Nil      => Nil
+//       case x :: Nil => x.map(Seq(_))
+//       case x :: xs =>
+//         val xsJoin = crossJoin(xs)
+//         for {
+//           i <- x
+//           j <- xsJoin
+//         } yield {
+//           Seq(i) ++ j
+//         }
+//     }
+//   override def flat(): AnnotationSeq = crossJoin(ttargets).map(r => this.copy(r.map(Seq(_))))
+  
+//   def toElementSeq(): Seq[TSeqElementAnno] = ttargets.flatMap(_.slice(0,1))
+//     //println("--------")
+//     /*ttargets.map{
+//       case Seq(AtmPropAnno(target)) => 
+//         {
+//           if(!renames(target).isEmpty)
+//           {
+//             println(target)
+//             println("***")
+//             println(renames(target))
+//           }
+//         }
+//         case Seq(TimeOpAnno(lowerCycles, upperCycles)) => 
+//           {
+//             println("Timep")
+//           }
+//     }*/
+    
+//     /*println("--------")
+//     ttargets.map{
+//       case AtmPropAnno(target) => 
+//         {
+//           if(!renames(target).isEmpty)
+//           {
+//             println(target)
+//             println("***")
+//             println(renames(target))
+//           }
+//         }
+//         case TimeOpAnno(lowerCycles, upperCycles) => 
+//           {
+//             println("Timep")
+//           }
+//     }
+//     Seq(this.copy(ttargets))*/
+  
+
+//   /*override def update(renames: RenameMap) = 
+//   {
+//     val tttemp : Seq[AtmPropAnno] = ttargets map  
+//     {
+//       case a:AtmPropAnno => {
+//         //println(t)
+//         //println(t.target.isInstanceOf[Target])
+//         //AtmPropAnno(renames(t.asInstanceOf[AtmPropAnno].target))
+//         val xx :Target = t.target
+//         val xxx = renames.get(xx.asInstanceOf[Target])
+//         a
+//       }
+//       case t:TimeOpAnno =>
+//       {
+
+//       }
+//     }
+
+//     //println(ttemp)
+//     /*ttargets.map{
+//       case AtmPropAnno(ap:Target) => 
+//         {
+//           println(ap)
+//           //val ttt :firrtl.annotations.Target = t1.asInstanceOf[AtmPropAnno].target
+//           val aap :firrtl.annotations.Target = ap.toTarget
+//           AtmPropAnno(ap)
+//           //AtmPropAnno(renames(aap))
+//         }
+//       case TimeOpAnno(lc,up) =>TimeOpAnno(lc,up)
+//     }*/
+//     Seq(this.copy(ttemp))
+//   }*/
+// }
+
+// /*case class SVAAnno(ts:TSeq)
+// {
+
+// }*/
diff --git a/src/main/scala/chiseltest/formal/TSeqElement.scala b/src/main/scala/chiseltest/formal/TSeqElement.scala
new file mode 100644
index 0000000..29ce605
--- /dev/null
+++ b/src/main/scala/chiseltest/formal/TSeqElement.scala
@@ -0,0 +1,72 @@
+// package chiseltest.formal
+
+// //import scala.language.implicitConversions
+// import chisel3._
+
+
+// trait svaElement extends TSeqElementAnno
+
+// case class AtmProp(signal:Bool) extends svaElement
+// //case class resetAnno(target:Target) extends TSeqElementAnno
+
+// trait s_seqElement extends svaElement
+// trait s_propElement extends svaElement
+
+// case class Leftbraket() extends svaElement
+// case class Rightbraket() extends svaElement
+
+
+// case class TimeOp(lowerCycles: Int, upperCycles: Int) extends s_seqElement
+// case class RepetOp(lowerBounds: Int, upperBounds: Int) extends svaElement
+
+// case class Implication() extends s_propElement
+// case class NotOp() extends s_propElement
+// case class FinallOp() extends s_propElement
+// case class GlobalOp() extends s_propElement
+// case class NextOp() extends s_propElement
+
+
+
+
+
+
+
+// object TTSeq
+// {
+//     implicit def uint2Atm(signal:Bool): TTSeq = new TTSeq(Seq(AtmProp(signal)))    
+//     //implicit def TSeqEle2Seq(te: svaElement): TTSeq = new TTSeq(Seq(te))
+//     def ap(signal:Bool): TTSeq = new TTSeq(Seq(AtmProp(signal)))
+//     def ###(lowerCycles: Int, upperCycles: Int): TTSeq = new TTSeq(Seq(TimeOp(lowerCycles, upperCycles)))
+//     def G: TTSeq = new TTSeq(Seq(GlobalOp()))
+//     def F: TTSeq = new TTSeq(Seq(FinallOp()))
+//     def printlnTSeq(t: TTSeq) = {println(t.s)}
+// }
+
+// class TTSeq(val s: Seq[svaElement])
+// {
+//     def G(t:TTSeq): TTSeq = new TTSeq((s :+ GlobalOp()) ++ t.s)
+//     def F(t:TTSeq): TTSeq = new TTSeq((s :+ FinallOp()) ++ t.s)
+
+//     def ap(t:TTSeq): TTSeq = 
+//     {
+        
+//         //val tte = AtmProp(signal)
+//         //val ttee = Seq(tte)
+//         //println(s"left: $s")
+//         //println(s"right: ${t.s}")
+//         new TTSeq(s ++ t.s)
+//     }
+//     // implicit def uint2Atm(signal:Bool): TTSeq = new TTSeq(Seq(AtmProp(signal)))
+//     def ###(lowerCycles: Int, upperCycles: Int): TTSeq = new TTSeq((s :+ TimeOp(lowerCycles, upperCycles)))
+//     def |->(t:TTSeq) = new TTSeq((s :+ Implication()) ++ t.s)
+//     /*def |->() = new TTSeq(s :+ Implication())*/
+//     //def |-> = new TTSeq(s :+ Implication()) 
+// }
+
+// /*class TNode(op: svaElement, left: TNode, right: TNode, isProperty:Boolean)
+// {
+//     def ap(tn: TNode): TNode = 
+//     {
+//         new TNode(AtmProp())
+//     }
+// }*/
\ No newline at end of file
diff --git a/src/main/scala/chiseltest/formal/TSequence.scala b/src/main/scala/chiseltest/formal/TSequence.scala
new file mode 100644
index 0000000..09d3dbc
--- /dev/null
+++ b/src/main/scala/chiseltest/formal/TSequence.scala
@@ -0,0 +1,172 @@
+// package chiseltest.formal
+
+// import chisel3._
+// import chiseltest.formal.past._
+// import scala.util.control.Breaks._
+// import scala.language.postfixOps
+// import scala.collection.mutable
+
+// object TSequence
+// {
+//   def apply(input: Seq[svaElement]) = {
+//     //val out = input.foldLeft(true.B)((p,q)=>parseTSequence(p,q))  
+//     val initInput = initTSeq(input)
+//     println(input.toString())
+//     println("---------")
+//     println(initInput.toString())
+//     val out = parseTSequence(initInput)
+
+//     WireInit(out)
+//   }
+
+
+//   private def initTSeq(input:Seq[svaElement]):Seq[svaElement] = 
+//   {
+//     import scala.collection.mutable.Seq
+//     var retSeq = scala.collection.mutable.Seq[svaElement]()
+//     for(i <- 0 until input.size)
+//     {
+//       if(retSeq.isEmpty)
+//       {
+//         retSeq :+= input(i)
+//       }
+//       else
+//       {
+//         input(i) match
+//         {
+//           case AtmProp(ap1) =>
+//           {
+//             retSeq(retSeq.size-1) match 
+//             {
+//               case AtmProp(ap2) => retSeq(retSeq.size-1) = AtmProp(ap1 & ap2)
+//               case _ => retSeq :+= AtmProp(ap1) 
+//             }
+//           }
+//           case TimeOp(lc1, hc1) =>
+//           {
+//             retSeq(retSeq.size-1) match 
+//             {
+//               case TimeOp(lc2, hc2) => retSeq(retSeq.size-1) = TimeOp(lc1+lc2, hc1+hc2)
+//               case _ => retSeq :+= TimeOp(lc1, hc1)
+//             }
+//           }
+//           case other => retSeq :+= other
+//         }
+//       }  
+//     }
+//     retSeq.toSeq
+//   }
+//   private def parseTSequenceNoImply(restSeq:Seq[svaElement]): (Int,Bool) =
+//   {
+//     if(restSeq.size == 0)
+//     {
+//       (0,true.B)
+//     }
+//     else if(restSeq.size == 1 && restSeq(0).isInstanceOf[AtmProp])
+//     {
+//       (1,restSeq(0).asInstanceOf[AtmProp].signal)
+//     }
+//     // else if(restSeq.size == 2 && restSeq(0).isInstanceOf[AtmProp] && restSeq(1).isInstanceOf[RepetOp])
+//     // {
+//     //   println("this!!!")
+//     //   val ap:AtmProp = restSeq(0).asInstanceOf[AtmProp]
+//     //   val repOp:RepetOp = restSeq(1).asInstanceOf[RepetOp]
+//     //   val lowerBounds = repOp.lowerBounds
+//     //   val upperBounds = repOp.upperBounds
+//     //   val beforeLower = (0 until lowerBounds).foldLeft(past(ap.signal,lowerBounds))((p, _) =>{ p && past(p) })
+//     //   (upperBounds, ( 0 until upperBounds-lowerBounds).foldLeft(ap.signal)((p, _) =>{ p || past(p) }) && beforeLower )  
+//     //   //(1,restSeq(0).asInstanceOf[AtmProp].signal)
+//     // }
+//     else
+//     {
+//       if(restSeq.size == 2 && restSeq(0).isInstanceOf[AtmProp] && restSeq(1).isInstanceOf[RepetOp])
+//       {
+//          val ap:AtmProp = restSeq(0).asInstanceOf[AtmProp]
+//          val repOp:RepetOp = restSeq(1).asInstanceOf[RepetOp]
+//          val lowerBounds = repOp.lowerBounds
+//          val beforeLower = (0 until lowerBounds-1).foldLeft(ap.signal)((p, _) =>{ p && past(p) })
+//          (lowerBounds,beforeLower)
+//       }
+//       else
+//       {
+//         val firstTO:Int = restSeq.indexWhere(_.isInstanceOf[TimeOp])
+//         //println(firstTO)
+//         //println(restSeq.toString())
+//         if(firstTO > 0 && restSeq(firstTO-1).isInstanceOf[RepetOp])
+//         {
+//           val ap:AtmProp = restSeq(firstTO-2).asInstanceOf[AtmProp]
+//           val repOp:RepetOp = restSeq(firstTO-1).asInstanceOf[RepetOp]
+//           val lowerBounds = repOp.lowerBounds
+//           val upperBounds = repOp.upperBounds
+
+//           val ret2 = parseTSequenceNoImply(restSeq.slice(firstTO,restSeq.size))
+//           val beforeLower = past((0 until lowerBounds-1).foldLeft(ap.signal)((p, _) =>{ p && past(p) }), (upperBounds-lowerBounds+ret2._1))
+//           var temp:mutable.Seq[Bool] = mutable.Seq(past(ap.signal,ret2._1))
+//           for(i <- 1 until upperBounds-lowerBounds)
+//           {
+//             temp :+= past(temp(i-1))
+//           }
+//           temp = temp.reverse
+//           for(i <- 1 until upperBounds-lowerBounds)
+//           {
+//             temp(i) = temp(i) && temp(i-1)
+//           }
+//           temp +:= true.B
+//           var temp2:mutable.Seq[Bool] = mutable.Seq(ret2._2)
+//           for(i <- 1 until upperBounds-lowerBounds+1)
+//           {
+//             temp2 :+= past(temp(i-1))
+//           }
+//           temp2 = temp2.reverse
+//           var partialExpr = temp(0) && temp2(0)
+//           for(i <- 1 until temp.size)
+//           {
+//             partialExpr = partialExpr || (temp(i) && temp2(i))
+//           }
+//           (ret2._1+upperBounds,partialExpr && beforeLower)
+//         }
+//         else
+//         {
+//           val timeOp = restSeq(firstTO).asInstanceOf[TimeOp]
+//           val ret1 = parseTSequenceNoImply(restSeq.slice(0,firstTO))
+//           val ret2 = parseTSequenceNoImply(restSeq.slice(firstTO+1,restSeq.size))
+//           val temp = ( 0 until timeOp.upperBounds-timeOp.lowerBounds).foldLeft(ret2._2)((p, _) =>{ p || past(p) })
+//           val total = past(ret1._2,ret2._1+timeOp.upperBounds) && temp
+//           (ret1._1+timeOp.upperBounds+ret2._1,total)
+//         }
+//       }
+//     }
+//   }
+
+//   private def parseTSequence(restSeq:Seq[svaElement]): (Bool) =
+//   {
+//     var lastImply:Int = restSeq.lastIndexOf(ImplicationOp())
+//     var ret = (0,true.B)
+//     if(lastImply < 0)
+//     {
+//       ret = parseTSequenceNoImply(restSeq)  
+//     }
+//     else
+//     {
+//       val deletedSeq = restSeq.slice(0,lastImply).filter(!_.isInstanceOf[ImplicationOp]) ++ restSeq.slice(lastImply,restSeq.size)
+//       val deletedSeq_ = initTSeq(deletedSeq)
+//       //println("-------")
+//       //println(deletedSeq.toString())
+//       lastImply = deletedSeq.lastIndexOf(ImplicationOp())
+//       val ret1 = parseTSequenceNoImply(deletedSeq.slice(0,lastImply))
+//       val ret2 = parseTSequenceNoImply(deletedSeq.slice(lastImply+1,deletedSeq.size))
+//       ret = (ret1._1+ret2._1-1,!past(ret1._2,ret2._1-1) || ret2._2)
+//     }
+//     val delay:UInt = (ret._1-1).asUInt
+//     println(s"delay is $delay")
+//     /*val cntReg = RegInit(0.U(delay.getWidth))
+//     when(cntReg < delay){
+//       cntReg := cntReg + 1.U
+//     }
+//     when(cntReg === delay){
+//       println("can reach delay")
+//     }
+//     || cntReg < delay*/
+//     ret._2 
+//   }
+// }
\ No newline at end of file
diff --git a/src/main/scala/chiseltest/formal/backends/IsModelChecker.scala b/src/main/scala/chiseltest/formal/backends/IsModelChecker.scala
index 9a167a5..9630bff 100644
--- a/src/main/scala/chiseltest/formal/backends/IsModelChecker.scala
+++ b/src/main/scala/chiseltest/formal/backends/IsModelChecker.scala
@@ -3,21 +3,29 @@
 package chiseltest.formal.backends
 
 import firrtl.backends.experimental.smt._
+import chiseltest.formal.{FormalOp, BoundedCheck}
 
 private[chiseltest] trait ModelCheckResult {
   def isFail: Boolean
   def isSuccess: Boolean = !isFail
 }
 private[chiseltest] case class ModelCheckSuccess() extends ModelCheckResult { override def isFail: Boolean = false }
+private[chiseltest] case class ModelCheckProve() extends ModelCheckResult {override def isFail: Boolean = false }
+
 private[chiseltest] case class ModelCheckFail(witness: Witness) extends ModelCheckResult {
   override def isFail: Boolean = true
 }
+private[chiseltest] case class ModelCheckFailNoWit() extends ModelCheckResult {
+  override def isFail: Boolean = true
+}
 
 private[chiseltest] trait IsModelChecker {
   def name: String
   val prefix:        String
   val fileExtension: String
   def check(sys: TransitionSystem, kMax: Int = -1): ModelCheckResult
+  // Scala disallows overloaded methods with default arguments
+  def check(sys: TransitionSystem, kMax: Int, algor:FormalOp): ModelCheckResult = check(sys, kMax)
 }
 
 private[chiseltest] case class Witness(
diff --git a/src/main/scala/chiseltest/formal/backends/Maltese.scala b/src/main/scala/chiseltest/formal/backends/Maltese.scala
index 5f244b8..19c212d 100644
--- a/src/main/scala/chiseltest/formal/backends/Maltese.scala
+++ b/src/main/scala/chiseltest/formal/backends/Maltese.scala
@@ -2,7 +2,8 @@
 
 package chiseltest.formal.backends
 
-import chiseltest.formal.backends.btor.BtormcModelChecker
+import chiseltest.formal._
+import chiseltest.formal.backends.btor.{BtormcModelChecker, PonoModelChecker}
 import chiseltest.formal.backends.smt._
 import chiseltest.formal.{DoNotModelUndef, DoNotOptimizeFormal, FailedBoundedCheckException}
 import firrtl._
@@ -30,6 +31,13 @@ case object Z3EngineAnnotation extends FormalEngineAnnotation
   */
 case object BtormcEngineAnnotation extends FormalEngineAnnotation
 
+/** Uses the pono model checker, similar to btormc 
+  * @note pono performs well in HWMCC 20, 
+  * @note it is generally faster than btormc but also needs to be built from source
+  * @note pono has implemented more model checking algorithm but can only check one assertion every call 
+  */
+case object PonoEngineAnnotation extends FormalEngineAnnotation
+
 /* Use a SMTLib based model checker with the yices2 SMT solver.
   * @note yices2 often performs better than Z3 or CVC4.
   * @note yices2 is not supported yet, because we have not figured out yet how to deal with memory initialization
@@ -50,7 +58,7 @@ case object BitwuzlaEngineAnnotation extends FormalEngineAnnotation
 
  Formal Verification based on the firrtl compiler's SMT backend and the maltese SMT libraries solver bindings. */
 private[chiseltest] object Maltese {
-  def bmc(circuit: ir.Circuit, annos: AnnotationSeq, kMax: Int, resetLength: Int = 0): Unit = {
+  def bmc(circuit: ir.Circuit, annos: AnnotationSeq, kMax: Int, resetLength: Int = 0, algor: FormalOp = BoundedCheck()): Unit = {
     require(kMax > 0)
     require(resetLength >= 0)
 
@@ -69,20 +77,69 @@ private[chiseltest] object Maltese {
     // perform check
     val checkers = makeCheckers(annos, targetDir)
     assert(checkers.size == 1, "Parallel checking not supported atm!")
-    checkers.head.check(sysInfo.sys, kMax = kMax + resetLength) match {
+
+    // currently, only pono can call k-Induction algorithm to prove the correctness of design
+    val proveChecker = checkers.head.isInstanceOf[PonoModelChecker] 
+    val proveAlgor = algor match { 
+      case _: KInductionCheck => true
+      case _: Ic3SaCheck => true
+      case _ => false
+    }
+    assert(!proveAlgor | proveChecker, s"${checkers.head.name} can't prove property!")
+
+    checkers.head.check(sysInfo.sys, kMax = kMax + resetLength, algor) match {
       case ModelCheckFail(witness) =>
         val writeVcd = annos.contains(WriteVcdAnnotation)
         if (writeVcd) {
-          val sim = new TransitionSystemSimulator(sysInfo.sys)
-          sim.run(witness, vcdFileName = Some((targetDir / s"${circuit.main}.bmc.vcd").toString))
-          val trace = witnessToTrace(sysInfo, witness)
-          val treadleState = prepTreadle(circuit, annos, modelUndef)
-          val treadleDut = TreadleBackendAnnotation.getSimulator.createContext(treadleState)
-          Trace.replayOnSim(trace, treadleDut)
+
+          val hasSVA = !noSva(sysInfo.sys)
+
+          println(s"hasSVA: ${hasSVA}")
+          
+          // violated safety property is from assertion, try to simulate
+          if(!hasSVA)
+          {
+            val sim = new TransitionSystemSimulator(sysInfo.sys)
+            sim.run(witness, vcdFileName = Some((targetDir / s"${circuit.main}.bmc.vcd").toString))
+            val trace = witnessToTrace(sysInfo, witness)
+            val treadleState = prepTreadle(circuit, annos, modelUndef)
+            val treadleDut = TreadleBackendAnnotation.getSimulator.createContext(treadleState)
+            Trace.replayOnSim(trace, treadleDut)
+          }
+          // if the violated safety property is from extended SVA-like assertion
+          // don't simulate, only output the lasso-shaped trace 
+          else
+          {
+            val inputNameMap = sysInfo.sys.inputs.map(_.name).map(name => name -> sysInfo.stateMap.getOrElse(name, name)).toMap
+            println(s"inputNameMap: ${inputNameMap}")
+            //- show aux state temporarily
+            val stateNameMap = sysInfo.sys.states.map(_.name).map(name => name -> {
+                sysInfo.stateMap.getOrElse(name, name)
+            }).toMap 
+
+            // val stateNameMap = sysInfo.sys.states.map(_.name).filter(sysInfo.stateMap.contains(_)).map(name => name -> {
+            //     sysInfo.stateMap(name)
+            // }).toMap  
+
+
+            println(s"states: ${sysInfo.sys.states}")
+            println(s"stateNameMap: ${stateNameMap}")
+            // to be optimized
+            // if triggered property is not liveness property, aux state variable can be hidden
+            val badString = witness.failed(0)
+            val triggerJustice = triggerJust(badString)
+            println(s"triggerJustice: ${triggerJustice}")
+
+            val sim = new TransitionSystemSimulator(sysInfo.sys, inputNameMap, stateNameMap, triggerJustice)
+            sim.run(witness, vcdFileName = Some((targetDir / s"${circuit.main}.bmc.vcd").toString))
+          }
         }
         val failSteps = witness.inputs.length - 1 - resetLength
         throw FailedBoundedCheckException(circuit.main, failSteps)
+      
+      case ModelCheckFailNoWit() => throw FailedBoundedCheckException(circuit.main, -1)
       case ModelCheckSuccess() => // good!
+      case ModelCheckProve() => // good!
     }
   }
 
@@ -125,8 +182,12 @@ private[chiseltest] object Maltese {
   private case class SysInfo(sys: TransitionSystem, stateMap: Map[String, String], memDepths: Map[String, Int])
 
   private def toTransitionSystem(circuit: ir.Circuit, annos: AnnotationSeq): SysInfo = {
+
     val logLevel = Seq() // Seq("-ll", "info")
+    // use other way instead of removing DeadCodeElimination
+    
     val opts: AnnotationSeq = if (annos.contains(DoNotOptimizeFormal)) Seq() else Optimizations
+    //SMTEmitter depends on FirrtlToTransitionSystem transform 
     val res = firrtlPhase.transform(
       Seq(
         RunFirrtlTransformAnnotation(Dependency(SMTLibEmitter)),
@@ -135,9 +196,10 @@ private[chiseltest] object Maltese {
       ) ++: logLevel ++: annos ++: LoweringAnnos ++: opts
     )
     val stateMap = FlattenPass.getStateMap(circuit.main, res)
+    println(s"stateMap: ${stateMap}")
     val memDepths = FlattenPass.getMemoryDepths(circuit.main, res)
     val sys = res.collectFirst { case TransitionSystemAnnotation(s) => s }.get
-
+    
     // print the system, convenient for debugging, might disable once we have done more testing
     if (true) {
       val targetDir = Compiler.requireTargetDir(annos)
@@ -150,6 +212,12 @@ private[chiseltest] object Maltese {
   private def noBadStates(sys: TransitionSystem): Boolean =
     sys.signals.count(_.lbl == IsBad) == 0
 
+  private def noSva(sys: TransitionSystem): Boolean =
+    sys.states.count(_.name.slice(0,9) == "assertSta") == 0 && sys.states.count(_.name.slice(0,9) == "assumeSta") == 0
+
+  private def triggerJust(badString: String): Boolean =
+    badString.slice(0,8) == "just2Bad"
+
   private def firrtlPhase = new FirrtlPhase
 
   private def makeCheckers(annos: AnnotationSeq, targetDir: os.Path): Seq[IsModelChecker] = {
@@ -159,6 +227,7 @@ private[chiseltest] object Maltese {
       case CVC4EngineAnnotation      => new SMTModelChecker(CVC4SMTLib)
       case Z3EngineAnnotation        => new SMTModelChecker(Z3SMTLib)
       case BtormcEngineAnnotation    => new BtormcModelChecker(targetDir)
+      case PonoEngineAnnotation => new PonoModelChecker(targetDir)
       case Yices2EngineAnnotation    => new SMTModelChecker(Yices2SMTLib)
       case BoolectorEngineAnnotation => new SMTModelChecker(BoolectorSMTLib)
       case BitwuzlaEngineAnnotation  => new SMTModelChecker(BitwuzlaSMTLib)
@@ -187,8 +256,7 @@ private[chiseltest] object Maltese {
       .toIndexedSeq
     val stateNames = sysInfo.sys.states
       .map(_.name)
-      // translate flattened state name to hierarchical path
-      .map(sysInfo.stateMap)
+      .map(name => sysInfo.stateMap.getOrElse(name, name))
       .toIndexedSeq
 
     Trace(
diff --git a/src/main/scala/chiseltest/formal/backends/TransitionSystemSimulator.scala b/src/main/scala/chiseltest/formal/backends/TransitionSystemSimulator.scala
index 474b9fd..aab22eb 100644
--- a/src/main/scala/chiseltest/formal/backends/TransitionSystemSimulator.scala
+++ b/src/main/scala/chiseltest/formal/backends/TransitionSystemSimulator.scala
@@ -10,8 +10,15 @@ import scala.collection.mutable
 
 private[chiseltest] class TransitionSystemSimulator(
   sys:               TransitionSystem,
+  inputNameMap:      Map[String,String] = Map[String,String](),
+  stateNameMap:      Map[String,String] = Map[String,String](),
+  val triggerJustice: Boolean = false,
   val maxMemVcdSize: Int = 128,
   printUpdates:      Boolean = false) {
+  
+  //- be catious! is it allowed to use cha and otiginal assertion together?
+  private val hasSVA = sys.states.count(_.name.slice(0,9) == "assertSta") != 0
+  private var loop = false
   private val (bvStates, arrayStates) = sys.states.partition(s => s.sym.isInstanceOf[BVSymbol])
   private val (bvSignals, arraySignals) = sys.signals.partition(s => s.e.isInstanceOf[BVExpr])
 
@@ -106,12 +113,40 @@ private[chiseltest] class TransitionSystemSimulator(
     withVcd: Boolean
   ): Unit = {
     // initialize vcd
+    println(s"hasSVA: ${hasSVA}")
     vcdWriter =
       if (!withVcd) None
       else {
         val vv = vcd.VCD(sys.name)
         vv.addWire("Step", 64)
-        allBV.foreach(s => vv.addWire(s.name, s.width))
+        if(hasSVA)
+        {
+          allBV.foreach(s => 
+          {
+            if(stateNameMap.contains(s.name) && s.name!="_resetCount")
+            {
+              println(s"s_name, s_width: ${s.name}, ${s.width}")
+              vv.addWire(stateNameMap(s.name), s.width)
+            }
+            else if(inputNameMap.contains(s.name))
+            {
+              // these
+              // if (!vv.isTempWire(inputNameMap(s.name)))
+              // println(s"add input: ${inputNameMap(s.name)}");
+              vv.addWire(inputNameMap(s.name), s.width)
+            }
+          })
+          if(triggerJustice)
+          {
+            println("with loop?")
+            vv.addWire("loop",1)
+          }
+        }
+        else
+        {
+          allBV.foreach(s => vv.addWire(s.name, s.width))
+        }
+
         observedMemories.foreach { m =>
           val depth = arrayDepth(m.indexWidth)
           (0 to depth).foreach(a => vv.addWire(s"${m.name}.$a", m.dataWidth))
@@ -152,14 +187,37 @@ private[chiseltest] class TransitionSystemSimulator(
     }
   }
 
-  private def step(index: Int, inputs: Map[Int, BigInt], expectedFailed: Option[Seq[String]] = None): Unit = {
+  private def step(index: Int, inputs: Map[Int, BigInt], bad: Int, expectedFailed: Option[Seq[String]] = None): Unit = {
     vcdWriter.foreach(_.wireChanged("Step", index))
 
     if (printUpdates) println(s"\nSTEP $index")
-
+    if(triggerJustice)
+    {
+      if(data(bvNameToIndex("seen_"+bad+"_")).equals(1))
+        loop = true
+    }
+    
     // dump state
     vcdWriter.foreach { v =>
-      bvStates.foreach { state => v.wireChanged(state.name, data(bvNameToIndex(state.name))) }
+      if(hasSVA)
+      {
+        if(triggerJustice)
+        {
+          if(loop)
+            v.wireChanged("loop",BigInt(1))
+          else
+            v.wireChanged("loop",BigInt(0))
+        }
+        println(s"bvStates: ${bvStates}")
+        bvStates.foreach { state => 
+          // if(stateNameMap.exists(_._1 == state.name))
+            // v.wireChanged(stateNameMap(state.name), data(bvNameToIndex(state.name))) }
+        v.wireChanged(state.name, data(bvNameToIndex(state.name))) }
+      }
+      else
+      {
+        bvStates.foreach { state => v.wireChanged(state.name, data(bvNameToIndex(state.name))) }
+      }
       observedMemories.foreach { mem =>
         val depth = arrayDepth(mem.indexWidth)
         val array = memories(arrayNameToIndex(mem.name))
@@ -167,11 +225,28 @@ private[chiseltest] class TransitionSystemSimulator(
       }
     }
 
-    // apply inputs
+    // apply inputs, it seems that the witness of inputs should not be array
     sys.inputs.zipWithIndex.foreach { case (input, ii) =>
       val value = inputs(ii)
       data(ii) = value
-      vcdWriter.foreach(_.wireChanged(input.name, value))
+      vcdWriter.foreach{v =>
+        if(hasSVA) 
+        // Some input will be hidden because it is illegal, 
+        // like invalid value emitted by validif  
+          v.wireChanged(inputNameMap(input.name), value)
+        else
+          v.wireChanged(input.name, value)
+      }
+      // if(hasSVA)
+      // {
+      //   if(inputNameMap.exists(_._1 == input.name))
+      //     println(s"what? ${inputNameMap(input.name)}; ${value}")
+      //     vcdWriter.foreach(_.wireChanged(inputNameMap(input.name), value))
+      // }
+      // else
+      // {
+      //   vcdWriter.foreach(_.wireChanged(input.name, value))
+      // }
       if (printUpdates) println(s"I: ${input.name} <- $value")
     }
 
@@ -181,7 +256,8 @@ private[chiseltest] class TransitionSystemSimulator(
         val value = eval(e)
         if (printUpdates) println(s"S: $name -> $value")
         data(bvNameToIndex(name)) = value
-        vcdWriter.foreach(_.wireChanged(name, value))
+        if(!hasSVA)
+          vcdWriter.foreach(_.wireChanged(name, value))
       case Signal(name, e: ArrayExpr, _) =>
         val value = evalArray(e)
         memories(arrayNameToIndex(name)) = value
@@ -214,6 +290,7 @@ private[chiseltest] class TransitionSystemSimulator(
       }
     }
 
+    // to be changed: pono may output witness with one more cycle!
     // check to see if any safety properties failed
     val failed = sys.signals
       .filter(_.lbl == IsBad)
@@ -259,17 +336,25 @@ private[chiseltest] class TransitionSystemSimulator(
   }
 
   def run(witness: Witness, vcdFileName: Option[String] = None): Unit = {
+
     init(witness.regInit, witness.memInit, withVcd = vcdFileName.nonEmpty)
+    
+    val badString = witness.failed(0)
+    println(s"badString: $badString")
+    val bad = if(triggerJustice) badString.slice(8,badString.size - 1).toInt else -1
+    println(s"bad: $bad")
+    
     witness.inputs.zipWithIndex.foreach { case (inputs, index) =>
       // on the last step we expect the bad states to be entered
       if (index == witness.inputs.size - 1) {
-        step(index, inputs, Some(witness.failed))
+        step(index, inputs, bad, Some(witness.failed))
       } else {
-        step(index, inputs)
+        step(index, inputs, bad)
       }
     }
     vcdFileName.foreach { ff =>
       val vv = vcdWriter.get
+      println(s"vv: ${vv}")
       vv.wireChanged("Step", witness.inputs.size)
       vv.incrementTime()
       vv.write(ff)
diff --git a/src/main/scala/chiseltest/formal/backends/btor/Btor2ModelChecker.scala b/src/main/scala/chiseltest/formal/backends/btor/Btor2ModelChecker.scala
index 919035b..5941b03 100644
--- a/src/main/scala/chiseltest/formal/backends/btor/Btor2ModelChecker.scala
+++ b/src/main/scala/chiseltest/formal/backends/btor/Btor2ModelChecker.scala
@@ -4,7 +4,9 @@ package chiseltest.formal.backends.btor
 
 import chiseltest.formal.backends._
 import firrtl.backends.experimental.smt._
+import chiseltest.formal.{FormalOp, BoundedCheck, KInductionCheck, Ic3SaCheck}
 
+import scala.util.control.Breaks._
 class BtormcModelChecker(targetDir: os.Path) extends IsModelChecker {
   override val fileExtension = ".btor2"
   override val name:   String = "btormc"
@@ -29,10 +31,120 @@ class BtormcModelChecker(targetDir: os.Path) extends IsModelChecker {
     // check to see if we were successful
     assert(r.exitCode == 0, s"We expect btormc to always return 0, not ${r.exitCode}. Maybe there was an error.")
     val isSat = res.nonEmpty && res.head.trim.startsWith("sat")
+    
+    if (isSat) {
+      val witness = Btor2WitnessParser.read(res, 1).head
+      ModelCheckFail(Btor2ModelChecker.convertWitness(sys, witness))
+    } else {
+      ModelCheckSuccess()
+    }
+  }
+}
+
+class PonoModelChecker(targetDir: os.Path) extends IsModelChecker
+{
+  override val fileExtension = ".btor2"
+  override val name:   String = "pono"
+  override val prefix: String = "pono"
+
+  override def check(sys: TransitionSystem, kMax: Int): ModelCheckResult = {
+    // serialize the system to btor2
+    val filename = sys.name + ".btor"
+    // btromc isn't happy if we include output nodes, so we skip them during serialization
+    val lines = Btor2Serializer.serialize(sys, skipOutput = true)
+    os.write.over(targetDir / filename, lines.mkString("", "\n", "\n"))
+
+    // execute model checker
+    val kmaxOpt = if (kMax > 0) Seq("-e","bmc","-k", kMax.toString, "--witness") else Seq()
+    val cmd = Seq("pono") ++ kmaxOpt ++ Seq(filename)
+    println(cmd)
+    val r = os.proc(cmd).call(cwd = targetDir, check = false)
+
+    // write stdout to file for debugging
+    val res = r.out.lines()
+    os.write.over(targetDir / (filename + ".out"), res.mkString("", "\n", "\n"))
+
+    // check to see if we were successful
+    // assert(r.exitCode == 0, s"We expect btormc to always return 0, not ${r.exitCode}. Maybe there was an error.")
+    val isSat = res.nonEmpty && res.head.trim.startsWith("sat")
+
+    val isSatNoWit = res.nonEmpty && res.head.trim.startsWith("IC3")
+    println(s"isSatNoWit: $isSatNoWit")
+    if (isSat) {
+      val witness = Btor2WitnessParser.read(res, 1).head
+      ModelCheckFail(Btor2ModelChecker.convertWitness(sys, witness))
+    } else if(isSatNoWit){
+      ModelCheckFailNoWit()
+    } else {
+      ModelCheckSuccess()
+    }
+  }
+
+  override def check(sys: TransitionSystem, kMax: Int, algor: FormalOp): ModelCheckResult = {
+    val filename = sys.name + ".btor"
+    // btromc isn't happy if we include output nodes, so we skip them during serialization
+    val lines = Btor2Serializer.serialize(sys, skipOutput = true)
+    os.write.over(targetDir / filename, lines.mkString("", "\n", "\n"))
+
+    // execute model checker
+    val badNum = sys.signals.count(_.lbl == IsBad)
+    val badSeq = Seq.range(0, badNum ,1)
+    var result: ModelCheckResult = ModelCheckSuccess()
+    breakable
+    {
+      badSeq.foreach{ 
+        badNu:Int =>
+        {
+          result = PonoModelChecker.checkProperty(targetDir, filename,sys, kMax, algor, badNu)
+          if(result.isInstanceOf[ModelCheckFail])
+            break()
+        }
+      }
+    }
+    result
+  }
+}
+
+object PonoModelChecker
+{
+  def engineCommand(algor:FormalOp) = {
+    algor match{
+      case x: BoundedCheck => "bmc"
+      case x: KInductionCheck => "ind"
+      case x: Ic3SaCheck => "ic3sa"
+      case _ => "bmc"
+    }
+  }
+
+  def checkProperty(targetDir: os.Path, filename:String, sys: TransitionSystem, kMax: Int, algor: FormalOp, badNum: Int):ModelCheckResult ={
+    val kmaxOpt = if (kMax > 0) Seq("-e",PonoModelChecker.engineCommand(algor), 
+    "-p", badNum.toString, "-k", kMax.toString, "--witness") else Seq()
+      // Seq("-e", PonoModelChecker.engineCommand(algor)) ++
+      // Seq("-p", badNum.toString) ++ 
+      // Seq("-k", kMax.toString, "--witness") else Seq()
+    val cmd = Seq("pono", "-v", "1") ++ kmaxOpt ++ Seq(filename)
+    
+    println(cmd)
+    val r = os.proc(cmd).call(cwd = targetDir, check = false)
+
+    // write stdout to file for debugging
+    val res = r.out.lines()
+    os.write.over(targetDir / (filename + ".out"), res.mkString("", "\n", "\n"))
+
+    // check to see if we were successful
+    // assert(r.exitCode == 0, s"We expect btormc to always return 0, not ${r.exitCode}. Maybe there was an error.")
+    val isSat = res.nonEmpty && res.head.trim.startsWith("sat")
+    val isUnSat = res.nonEmpty && res.head.trim.startsWith("unsat")
+    val isSatNoWit = res.nonEmpty && res.head.trim.startsWith("IC3")
+    println(s"isSatNoWit: $isSatNoWit")
 
     if (isSat) {
       val witness = Btor2WitnessParser.read(res, 1).head
       ModelCheckFail(Btor2ModelChecker.convertWitness(sys, witness))
+    } else if (isUnSat) {
+      ModelCheckProve()
+    } else if(isSatNoWit){
+      ModelCheckFailNoWit()
     } else {
       ModelCheckSuccess()
     }
diff --git a/src/main/scala/chiseltest/formal/hoaParser.scala b/src/main/scala/chiseltest/formal/hoaParser.scala
new file mode 100644
index 0000000..3974388
--- /dev/null
+++ b/src/main/scala/chiseltest/formal/hoaParser.scala
@@ -0,0 +1,316 @@
+package chiseltest.formal
+
+import jhoafparser.ast.AtomAcceptance
+import jhoafparser.ast.AtomLabel
+import jhoafparser.ast.BooleanExpression
+import jhoafparser.consumer.HOAConsumer
+import jhoafparser.consumer.HOAConsumerException
+import jhoafparser.parser.HOAFParser
+import jhoafparser.parser.generated.ParseException
+import scala.collection.mutable
+import java.util.stream.IntStream
+import collection.JavaConverters._
+import scala.math._
+import net.sf.javabdd.BDD
+
+
+class hoaParser extends HOAConsumer{
+  
+    var bdd:BDDManager = new BDDManager()
+
+    // the state which index is stateNum is a default dead state to help deterministic... 
+    var stateNum: Int = 0
+    var stateBits: Int = 0
+    var initState: Int = 0
+    var accStates: Seq[Int] = Seq()
+
+    var transitionFunc: mutable.Map[Int, mutable.Map[BDD, mutable.Set[Integer]]] = mutable.Map[Int, mutable.Map[BDD, mutable.Set[Integer]]]()
+    var int2Ap: mutable.Map[Int,String] = mutable.Map[Int,String]()
+    var int2Aux: mutable.Map[Int,String] = mutable.Map[Int,String]()
+    
+    var apNum: Int = 0
+    var auxVarNum: Int = 0
+    
+    def int2Bdd(i: Int, auxVars: Seq[Int]): BDD =
+    {
+        val bin = i.toBinaryString.reverse
+        val auxVars_ : Seq[BDD] = auxVars.zipWithIndex.collect{
+            case Tuple2(a:Int,b:Int) => 
+            {
+                /*println(a,b)
+                println(bin.length())*/
+                if(b < bin.length() && bin(b) == '1')
+                    bdd.ithVar(a)    
+                else
+                    (bdd.ithVar(a)).not()
+            }
+        }
+        //auxVarNum = if(auxVars_. > auxVarNum) {auxVars_} else() {auxVarNum}
+        
+        if(auxVars_.size == 1)
+            {auxVars_(0)}
+        else
+        {
+            val auxVars__ = auxVars_.slice(1,auxVars_.size)
+            auxVars__.foldLeft(auxVars_(0))((a,b)=> a.and(b))
+        }
+    }
+
+    def addAuxVar(): Unit =
+    {
+        for(i <- 0 until stateNum)
+        {   
+            println(s"this state: ${i}")
+            var trans: mutable.Map[BDD, mutable.Set[Integer]] = transitionFunc(i)
+            val neededAuxVar:Int = ceil(log(trans.size)).toInt
+            // add AuxVar when there are more than 1 outgoing-edge
+            if(neededAuxVar > 0)
+            {
+                if(neededAuxVar > auxVarNum)
+                {
+                    bdd.extVarNum(neededAuxVar - auxVarNum)
+                    auxVarNum = neededAuxVar   
+                }
+                val varSeq = (apNum until apNum + neededAuxVar).toSeq
+                val trans_ = mutable.Map() ++ trans.keys.zipWithIndex.collect{
+                    case Tuple2(a:BDD, b:Int) =>
+                        println(s"map[BDD,Int] $a, $b")
+                        Tuple2(a.and(int2Bdd(b,varSeq)),trans.get(a).get)
+                }.toMap
+                transitionFunc(i)  = trans_
+                println(s"new trans: ${trans_}")  
+            }
+        }
+    }
+
+    //check all accStates are badState
+    def badAccs(): Boolean = {
+        val accISBad = accStates.map{
+            i:Int =>
+            {
+                // println(transitionFunc(i).size == 1)
+                // println(transitionFunc(i).last._1.isOne())
+                // println(transitionFunc(i).last._2)
+                // println(i)
+                transitionFunc(i).size == 1 & transitionFunc(i).last._1.isOne() &
+                transitionFunc(i).last._2.size == 1 & transitionFunc(i).last._2.head == i
+            }
+        }
+        println(s"accStates: $accStates")
+        println(accISBad.toSeq)
+        accISBad.foldLeft(true)((a,b)=> a & b)
+        false
+    }
+
+    //- old_addAuxVar is collated with old_partialDeterministic
+    def old_addAuxVar(): Unit =
+    {
+        /*val ran:Seq[Int] = (2 until 6).toSeq
+        val testAbove = int2Bdd(13,ran)
+        println(testAbove)*/
+        for(i <- 0 until stateNum)
+        {
+            var trans: mutable.Map[BDD, mutable.Set[Integer]] = transitionFunc(i)
+            var trans_ = trans.clone()
+            //var usedAuxVar: Int = 0
+            for((k,v) <- trans)
+            {
+                if(v.size > 1)
+                {
+                  //  println(s"need aux: $k")
+                    trans_.remove(k)
+                    val neededAuxVar:Int = ceil(log(v.size)).toInt
+                    if(neededAuxVar > auxVarNum)
+                    {
+                        bdd.extVarNum(neededAuxVar - auxVarNum)
+                        auxVarNum = neededAuxVar   
+                    }
+                    /*println(s"v_size: ${ceil(log(v.size))}")
+                    println(s"neededAuxVar: ${neededAuxVar}")*/
+                    val varSeq = (apNum until apNum + neededAuxVar).toSeq
+                    trans_ ++= mutable.Map() ++ v.zipWithIndex.collect{
+                        case Tuple2(a:Integer, b:Int) =>
+                            Tuple2(k.and(int2Bdd(b,varSeq)),mutable.Set(a))
+                    }.toMap
+                  //  println(s"need aux: $trans_")
+                }
+            }
+            transitionFunc(i)  = trans_
+        }
+    }
+
+    //this process should be after parsing
+    //the complexity is high and highly related to the output automata of spot
+    def old_partialDeterministic(): Unit = 
+    {
+        for(i <- 0 until stateNum)
+        {
+            var mutualBdds: mutable.Set[BDD] = mutable.Set[BDD]()
+            var trans: mutable.Map[BDD, mutable.Set[Integer]] = transitionFunc(i)
+
+            if(trans.isEmpty)
+                throw new UnsupportedOperationException("There exists an illegal state")
+
+            var updatedEdge: mutable.Map[BDD, mutable.Set[Integer]] = mutable.Map[BDD, mutable.Set[Integer]]()
+
+            for((k,v) <- trans)
+            {
+                if(mutualBdds.isEmpty)
+                {
+                    mutualBdds += k
+                    updatedEdge += (k -> v)
+                } 
+                else
+                {
+                    var mutualBddsCopy = mutualBdds.clone()
+                    var isExclusive = true
+                    for(e <- mutualBddsCopy)
+                    {
+                        if(!(e.and(k).isZero()))
+                        {
+                            val oldEdge = updatedEdge(e)
+                            
+                            mutualBdds.remove(e)
+                            updatedEdge = updatedEdge-(e)
+
+                            isExclusive = false
+                            mutualBdds += e.and(k)
+                            updatedEdge += (e.and(k) -> (oldEdge ++ v))
+
+                            if(!(e.and(k.not()).isZero()))
+                            {
+                                mutualBdds += e.and(k.not())
+                                updatedEdge += (e.and(k.not()) -> oldEdge)
+                            }
+                            if(!(k.and(e.not()).isZero))
+                            {
+                                mutualBdds += k.and(e.not())
+                                updatedEdge += (k.and(e.not()) -> v)
+                            }                      
+                        }
+                    }
+                    if(isExclusive)
+                    {
+                        mutualBdds += k
+                        updatedEdge += (k -> v)
+                    }
+                }
+            }
+          //  println(s"i=$i: $mutualBdds")
+            var isFull = mutualBdds.fold(bdd.zero())((e1,e2)=>e1.or(e2))
+
+            //println(isTrue)
+            if(!isFull.isOne())
+                updatedEdge += (isFull.not() ->  mutable.Set(stateNum))
+            transitionFunc(i) = updatedEdge            
+        }
+        transitionFunc += stateNum -> mutable.Map(bdd.one() -> mutable.Set(stateNum))
+    }
+
+    @throws(classOf[HOAConsumerException])
+    override def setNumberOfStates(numberOfStates: Int): Unit =
+    {
+        stateNum = numberOfStates
+        
+        stateBits = ceil(log(numberOfStates.toDouble + 1) / log(2)).toInt
+      //  println(s"log(numberOfStates.toDouble + 1): ${log(numberOfStates.toDouble + 1)}")
+      //  println(s"numberOfStates: $numberOfStates, stateBits: $stateBits")
+    }
+
+    @throws(classOf[HOAConsumerException])
+    override def addEdgeWithLabel(stateId: Int, labelExpr: BooleanExpression[AtomLabel], conjSuccessors: java.util.List[Integer], accSignature: java.util.List[Integer]): Unit =
+    {
+        if(conjSuccessors.size() != 1)
+            throw new UnsupportedOperationException("Successor conjunction is not allowed")
+        assert(labelExpr != null)
+
+        var targetId:Integer = conjSuccessors.get(0)
+        if(labelExpr.getAtom() != null && labelExpr.getAtom().isAlias() )
+            throw new UnsupportedOperationException("Unsupported label booleanExpression")
+        else
+        {
+            val labelBdd = bdd.boolExpr2Bdd(labelExpr)
+            if(!transitionFunc.contains(stateId))
+                transitionFunc += stateId -> mutable.Map(labelBdd -> mutable.Set(targetId))
+            else if(!transitionFunc(stateId).contains(labelBdd))
+                transitionFunc(stateId) += (labelBdd -> mutable.Set(targetId))
+            else 
+                transitionFunc(stateId)(labelBdd) ++= mutable.Set(targetId)
+        }
+    }
+
+    @throws(classOf[HOAConsumerException])
+    override def setAPs(aps: java.util.List[String]): Unit = 
+    {
+        int2Ap = mutable.Map() ++ aps.asScala.zipWithIndex.map(a => (a._2, a._1)).toMap
+        apNum = aps.size()
+        if(apNum == 0)
+            throw new UnsupportedOperationException("BA is empty, this assertion is meaningless")
+        bdd.setVarNum(apNum)
+    }
+
+    @throws(classOf[HOAConsumerException])
+    override def addStartStates(stateConjunction: java.util.List[Integer]): Unit = 
+    {
+        if(stateConjunction.size() != 1)
+            throw new UnsupportedOperationException("only allow one initial state")
+        initState = stateConjunction.get(0)
+    }
+
+    @throws(classOf[HOAConsumerException])
+    override def addAlias(name: String, labelExpr: BooleanExpression[AtomLabel]): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def addEdgeImplicit(x: Int, x2: java.util.List[Integer], x3: java.util.List[Integer]): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def addMiscHeader(x$1: String,x$2: java.util.List[Object]): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def addProperties(x$1: java.util.List[String]): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def addState(id: Int, info: String, labelExpression: jhoafparser.ast.BooleanExpression[jhoafparser.ast.AtomLabel], accSignature: java.util.List[Integer]): Unit = 
+    {
+        if(accSignature != null && accSignature.size() > 0)
+        {
+            accStates :+= id
+        }
+    }
+
+    @throws(classOf[HOAConsumerException])
+    override def parserResolvesAliases(): Boolean = {
+        false
+    }
+
+    @throws(classOf[HOAConsumerException])
+    override def provideAcceptanceName(x$1: String,x$2: java.util.List[Object]): Unit = {}
+   
+    @throws(classOf[HOAConsumerException])
+    override def setAcceptanceCondition(x$1: Int,x$2: jhoafparser.ast.BooleanExpression[jhoafparser.ast.AtomAcceptance]): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def setName(x$1: String): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def setTool(x$1: String,x$2: String): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def notifyHeaderStart(version: String): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def notifyAbort(): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def notifyBodyStart(): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def notifyEnd(): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def notifyEndOfState(stateId: Int): Unit = {}
+
+    @throws(classOf[HOAConsumerException])
+    override def notifyWarning(warning: String): Unit = {}
+
+}
diff --git a/src/main/scala/chiseltest/formal/package.scala b/src/main/scala/chiseltest/formal/package.scala
index 4dfc86d..d046bd3 100644
--- a/src/main/scala/chiseltest/formal/package.scala
+++ b/src/main/scala/chiseltest/formal/package.scala
@@ -7,6 +7,7 @@ package object formal {
   val CVC4EngineAnnotation = backends.CVC4EngineAnnotation
   val Z3EngineAnnotation = backends.Z3EngineAnnotation
   val BtormcEngineAnnotation = backends.BtormcEngineAnnotation
+  val PonoEngineAnnotation = backends.PonoEngineAnnotation
   val BitwuzlaEngineAnnotation = backends.BitwuzlaEngineAnnotation
   val MagicPacketTracker = vips.MagicPacketTracker
 }
diff --git a/src/main/scala/chiseltest/formal/svaAnno.scala b/src/main/scala/chiseltest/formal/svaAnno.scala
new file mode 100644
index 0000000..468fde5
--- /dev/null
+++ b/src/main/scala/chiseltest/formal/svaAnno.scala
@@ -0,0 +1,633 @@
+// important: solve problems casued by extra clock information and module information...
+// important: reconsider the semantics of reset: G (!reset -> p)
+package chiseltest.formal
+
+import scala.collection.mutable
+import chisel3._
+import chisel3.experimental.{ChiselAnnotation,annotate,RunFirrtlTransform}
+import firrtl.annotations.{Annotation, ReferenceTarget, SingleTargetAnnotation, Target, MultiTargetAnnotation,NoTargetAnnotation}
+import firrtl.options.StageUtils
+import firrtl.RenameMap
+import org.json4s.JValue
+import firrtl._
+import firrtl.ir.Expression
+
+import scala.collection.Traversable
+import scala.reflect.runtime.{universe => ru}
+import javax.swing.text.html.HTMLEditorKit.Parser
+import scala.util.parsing.combinator._
+
+// class(trait) hierarch:
+// sva_op(only for parser) - seq_op -repe_op, time_delay_op
+
+// sva_node: for parser and annotation sequence
+//  -sva_seq: describe seq in parser
+//  -sva_prop: describe prop in parser 
+//  -svaElementAnno: describe the prop in annotation sequence
+//    -un_op_node: uniary operator
+//      +sva_seq: un_op_seq
+//      +sva_pro: un_op_prop
+//    -bin_op_node: binary operator
+//      +sva_seq: bin_op_seq
+//      +sva_pro: bin_op_prop
+
+//Only for parser, suffixed with "op" 
+trait sva_op
+trait seq_op extends sva_op
+case class repe_op(lowerBound:Int, upperBound:Int) extends seq_op
+case class time_delay_op(lowerBound:Int, upperBound:Int) extends seq_op
+
+//For parser and annotation sequence
+trait sva_node
+{
+  // For serialization: omit the information of its children
+  def eraseChildren(): sva_node = this
+  def setChildren(s: Seq[sva_node]): Unit = {}
+  // Unfold the tree structure and return a sequence
+  def treeSerialize(): Seq[sva_node] = 
+  {
+    this match
+    {
+      case n:atom_prop_node => Seq(this.eraseChildren())
+      case n:bin_op_node[_,_] => 
+      {
+        val lSeq = n.lchild.treeSerialize()
+        val rSeq = n.rchild.treeSerialize()
+        Seq(n.eraseChildren()) ++ lSeq ++ rSeq
+      }
+      case n:un_op_node[_] => 
+      {
+        val lSeq = n.child.treeSerialize()
+        Seq(n.eraseChildren()) ++ lSeq 
+      }
+      case n:constantTrue => Seq(constantTrue())
+      case n:constantFalse => Seq(constantFalse())
+      case n => {println(s"Unexcepted error? $n"); Seq(n.eraseChildren())}
+    }
+  }
+
+  def treeDeSerialize(s:Seq[sva_node]): Tuple2[sva_node, Int] =
+  {
+    this match 
+    {
+      case n:atom_prop_anno => (this, 1)
+      case n:constantTrue => (this, 1)
+      case n:constantFalse => (this, 1)
+      case n: un_op_node[_] => 
+      {
+        val (tChild, costLength) = s(0).treeDeSerialize(s.tail)
+        this.setChildren(Seq(tChild))
+        (this, costLength + 1)
+      }  
+      case n: bin_op_node[_,_] => 
+      {
+        val (tChild1, costLength1) = s(0).treeDeSerialize(s.tail)
+        val (tChild2, costLength2) = s(costLength1).treeDeSerialize(s.slice(costLength1+1,s.size)) 
+        this.setChildren(Seq(tChild1, tChild2))
+        (this, costLength1 + costLength2 + 1)
+      } 
+    }
+  }
+}
+
+trait svaElementAnno extends sva_node
+{
+  def toPSL(rename2p: Map[Target,String]): String
+  def toSVA(f: Target => Expression): Seq[Any]
+}
+trait un_op_node[T<: sva_node] extends svaElementAnno
+{
+  var child: T
+  def opPSLL: String
+  def opPSLR: String
+  def opSVAL: String
+  def opSVAR: String
+  override def eraseChildren(): sva_node = { child = null.asInstanceOf[T]; this }
+  override def setChildren(s: Seq[sva_node])= 
+  {
+    assert(s.size == 1)
+    this.child = s(0).asInstanceOf[T]
+  }
+  override def toPSL(rename2p: Map[Target,String]): String = "(" + opPSLL + child.asInstanceOf[svaElementAnno].toPSL(rename2p) + opPSLR +")" +
+    ""
+  override def toSVA(f: Target => Expression): Seq[Any] = Seq("(", opSVAL, child.asInstanceOf[svaElementAnno].toSVA(f) , opSVAR ,")")
+}
+
+trait bin_op_node[T1<: sva_node, T2<: sva_node] extends svaElementAnno
+{
+  var lchild: T1
+  var rchild: T2
+  def opPSL: String
+  def opSVA: String
+
+  override def eraseChildren(): sva_node = { lchild = null.asInstanceOf[T1]; rchild = null.asInstanceOf[T2]; this }
+  override def setChildren(s: Seq[sva_node])= 
+  {
+    assert(s.size == 2)
+    this.lchild = s(0).asInstanceOf[T1]
+    this.rchild = s(1).asInstanceOf[T2]
+  }
+
+  // A strange error when using match-case 
+  override def toPSL(rename2p: Map[Target,String]): String = 
+  {
+    if(this.isInstanceOf[over_impl_prop])
+    {
+      "({ "+ lchild.asInstanceOf[svaElementAnno].toPSL(rename2p) + " }" + opPSL + rchild.asInstanceOf[svaElementAnno].toPSL(rename2p) + ")"
+    }
+    else
+    {
+      "(" + lchild.asInstanceOf[svaElementAnno].toPSL(rename2p) + opPSL + rchild.asInstanceOf[svaElementAnno].toPSL(rename2p) + ")"
+    }
+  }
+
+  override def toSVA(f: Target => Expression): Seq[Any] = 
+  {
+    if(this.isInstanceOf[over_impl_prop])
+    {
+      Seq(lchild.asInstanceOf[svaElementAnno].toSVA(f) , opSVA , rchild.asInstanceOf[svaElementAnno].toSVA(f))
+    }
+    else
+    {
+      Seq(lchild.asInstanceOf[svaElementAnno].toSVA(f) , opSVA , rchild.asInstanceOf[svaElementAnno].toSVA(f))
+    }
+  }
+}
+
+trait sva_seq extends sva_node
+trait un_op_seq extends un_op_node[sva_seq] with sva_seq
+trait bin_op_seq extends bin_op_node[sva_seq, sva_seq] with sva_seq
+
+case class constantTrue() extends sva_seq with svaElementAnno
+{
+  override def toPSL(rename2p: Map[Target,String]): String = "1" 
+  override def toSVA(f: Target => Expression): Seq[Any] = Seq("1")
+}
+case class constantFalse() extends sva_seq with svaElementAnno
+{
+  override def toPSL(rename2p: Map[Target,String]): String = "0" 
+  override def toSVA(f: Target => Expression): Seq[Any] = Seq("0")
+}
+case class atom_prop_node(signal:Bool) extends sva_seq
+case class atom_prop_anno(signal:ReferenceTarget) extends sva_seq with svaElementAnno
+{
+  override def toPSL(rename2p: Map[Target,String]): String = rename2p(signal) 
+  override def toSVA(f: Target => Expression): Seq[Any] = Seq(f(signal))
+}
+case class repe_seq(lowerBound:Int, upperBound:Int, var child:sva_seq) extends un_op_seq
+{
+  override def opPSLL: String = "" 
+  override def opPSLR: String = 
+  {
+    val upperBounds:String = if(upperBound == -1) "$" else upperBound.toString()
+    "[*" + lowerBound + ":" + upperBounds + "]"
+  }
+
+  override def opSVAL: String = opPSLL
+  override def opSVAR: String = opPSLR
+}
+case class time_delay_seq(lowerBound:Int, upperBound:Int, var lchild:sva_seq, var rchild:sva_seq) extends bin_op_seq
+{
+  override def opPSL: String = 
+  {
+    val upperBounds:String = if(upperBound == -1) "$" else upperBound.toString()
+    "##[" + lowerBound + ":" + upperBounds + "]"
+  }
+  override def opSVA: String = opPSL
+}
+case class and_seq(var lchild:sva_seq, var rchild:sva_seq) extends bin_op_seq
+{
+  override def opPSL: String = " && "
+  override def opSVA: String = " and "
+}
+case class or_seq(var lchild:sva_seq, var rchild:sva_seq) extends bin_op_seq
+{
+  override def opPSL: String = " || "
+  override def opSVA: String = " or "
+}
+
+
+trait sva_pro extends sva_node
+trait un_op_pro extends un_op_node[sva_pro] with sva_pro
+trait bin_op_pro extends bin_op_node[sva_pro, sva_pro] with sva_pro
+
+case class not_prop(var child:sva_pro) extends un_op_pro
+{
+  override def opPSLL: String = "! "
+  override def opPSLR: String = ""
+  override def opSVAL: String = "not"
+  override def opSVAR: String = opPSLR
+}
+case class next_prop(var child:sva_pro) extends un_op_pro
+{
+  override def opPSLL: String = "X"
+  override def opPSLR: String = ""
+  override def opSVAL: String = "nexttime"
+  override def opSVAR: String = opPSLR
+}
+case class and_prop(var lchild:sva_pro, var rchild:sva_pro) extends bin_op_pro
+{
+  override def opPSL: String = " && "
+  override def opSVA: String = " and "
+}
+case class or_prop(var lchild:sva_pro, var rchild:sva_pro) extends bin_op_pro
+{
+  override def opPSL: String = " || "
+  override def opSVA: String = " or "
+}
+case class until_prop(var lchild:sva_pro, var rchild:sva_pro) extends bin_op_pro
+{
+  override def opPSL: String = " U "
+  override def opSVA: String = "until"
+}
+case class impl_prop(var lchild:sva_pro, var rchild:sva_pro) extends bin_op_pro
+{
+  override def opPSL: String = " -> "
+  override def opSVA: String = " implies "
+}
+case class glo_prop(var child:sva_pro) extends un_op_pro
+{
+  override def opPSLL: String = "G "
+  override def opPSLR: String = ""
+
+  override def opSVAL: String = "always"
+  override def opSVAR: String = ""
+}
+case class fina_prop(var child:sva_pro) extends un_op_pro
+{
+  override def opPSLL: String = "F "
+  override def opPSLR: String = ""
+
+  override def opSVAL: String = "s_eventually"
+  override def opSVAR: String = ""
+}
+
+case class over_impl_prop(var lchild:sva_seq, var rchild:sva_pro) extends bin_op_node[sva_seq, sva_pro] with sva_pro
+{
+  override def opPSL: String = " []-> "
+  override def opSVA: String = " |-> "
+}
+case class prompt_prop(var child:sva_seq) extends un_op_node[sva_seq] with sva_pro
+{
+  override def opPSLL: String = ""
+  override def opPSLR: String = ""
+
+  override def opSVAL: String = ""
+  override def opSVAR: String = ""
+  override def toPSL(rename2p: Map[Target,String]): String = 
+  {
+    "(" + "{" + child.asInstanceOf[svaElementAnno].toPSL(rename2p) + "}" +")" 
+  }
+}
+// case class NoneOp() extends sva_node
+
+
+// case class NoneOp() extends sva_node
+// case class ResetAnno(target:Target) extends sva_node
+// {
+//   override def toPSL(rename2p: Map[Target,String]): String = {println("Misuse this API"); ""}
+//   override def toSVA(f: Target => Expression): Seq[Any] = {println("Misuse this API"); ""}
+// }
+
+trait targetAnno extends sva_node
+{
+  val target: Target
+}
+
+case class ResetAnno(target:Target) extends targetAnno
+case class EnableAnno(target:Target) extends targetAnno
+case class ClockAnno(target:Target) extends targetAnno
+case class ModuleAnno(target:Target) extends targetAnno
+
+class sva_tree(o:Object) extends JavaTokenParsers {
+  def prop: Parser[sva_pro] = 
+  (
+      prop6 ^^{case p => p}
+  )
+  def prop6: Parser[sva_pro] =
+  (
+      "G"~>prop6                    ^^ {case p:sva_pro => println(s"prop5: $p"); glo_prop(p)}
+    | "F"~>prop6                    ^^ {case p:sva_pro => println(s"prop5: $p"); fina_prop(p)}
+    | prop5                         ^^ {case p:sva_pro => println(s"prop5: $p"); p}
+  )
+  def prop5: Parser[sva_pro] =
+  (
+    seq~"|->"~prop6                 ^^ {case ~(~(p1,o),p2) =>  println(s"prop4: $p1 $p2"); over_impl_prop(p1,p2)}
+    | prop4                         ^^ {case p:sva_pro => println(s"prop4: $p"); p}
+  )
+  def prop4: Parser[sva_pro] =
+  (
+      prop3~"U"~prop6               ^^ {case ~(~(p1,o),p2) =>  println(s"prop3: $p1 $p2"); until_prop(p1,p2)}
+    | prop3~"->"~prop6              ^^ {case ~(~(p1,o),p2) =>  println(s"prop3: $p1 $p2"); impl_prop(p1,p2)}
+    | prop3                         ^^ {case p:sva_pro =>  p}
+  )
+  def prop3: Parser[sva_pro] =
+  (
+    prop2~opt("||"~prop6)           ^^ {case ~(p1,Some(~(o,p2))) =>  or_prop(p1,p2)
+                                      case ~(p,None) => p}
+  )
+  def prop2: Parser[sva_pro] =
+  (
+    prop1~opt("&&"~prop6)           ^^ {case ~(p1,Some(~(o,p2))) =>  and_prop(p1,p2)
+                                      case ~(p,None) => p}
+  )
+  def prop1: Parser[sva_pro] = 
+  (
+      "!"~>prop1 ^^ {case p:sva_pro => println(s"prop1: $p");not_prop(p)}  
+    | "X"~>prop1 ^^ {case p:sva_pro => println(s"prop1: $p");next_prop(p)}
+    | seq       ^^ {case s:sva_seq => println(s"prop1: $s");prompt_prop(s)}
+    | "("~>prop<~")" ^^{case p:sva_pro => println(s"prop1: $p");p}
+    // |  "!"~>prop ^^ {case p:sva_pro => println(s"prop1: $p");not_prop(p)}  
+    // | "X"~>prop ^^ {case p:sva_pro => println(s"prop1: $p");next_prop(p)}
+    // | "G"~>prop ^^ {case p:sva_pro => println(s"prop1: $p");glo_prop(p)}
+  )
+
+  // Use seq1,seq2 to maintain the precedence: ## > && > or
+  // Unary sequence operator repetition has highest priority (in repe) 
+  def seq: Parser[sva_seq] =
+  (
+    opt(time_delay)~seq3         ^^ {case ~(Some(t),s) =>  time_delay_seq(t.lowerBound,t.upperBound,constantTrue(),s)
+                                        case ~(None,s) => s
+                                            }
+  )
+  def seq3: Parser[sva_seq] = 
+  (
+    seq2~opt("||"~seq)              ^^ {case ~(s1,Some(~(o,s2))) =>  or_seq(s1,s2)
+                                      case ~(s,None) => println(s"seq2: $s"); s
+                                      case x => println(x); new atom_prop_node(false.asBool)}
+  )
+  def seq2: Parser[sva_seq] = 
+  (
+    seq1~opt("&&"~seq2)             ^^ {case ~(s1,Some(~(o,s2))) =>  and_seq(s1,s2)
+                                      case ~(s,None) => s
+                                      case x => println(x); new atom_prop_node(false.asBool)}
+    // seq1 ^^ {case p => println(s"seq: $p"); p}
+  )
+  def seq1: Parser[sva_seq] = 
+  (
+    repe~opt(time_delay~seq1)       ^^ {case ~(r,Some(~(t,s))) =>  time_delay_seq(t.lowerBound,t.upperBound,r,s)
+                                      case ~(r,None) => r
+                                      case x => println(x); new atom_prop_node(false.asBool)}
+  )
+    
+  //##m, ##[m:n], ##[*], ##[+]
+  def time_delay: Parser[time_delay_op] = 
+  (
+    // "##"~(wholeNumber | "["~(wholeNumber~":"~wholeNumber | wholeNumber~":"~"$" | "*" | "+")~"]") 
+      "##"~>tuple2                  ^^ {t => time_delay_op(t._1,t._2)}
+    | "##"~"["~>tuple2<~"]"         ^^ {t => time_delay_op(t._1,t._2)}
+  )
+  def repe: Parser[sva_seq] = 
+  (
+      atom_prop~opt(repe_range)     ^^ {case ~(ap,Some(r)) => println(s"repe $ap, $r"); repe_seq(r.lowerBound,r.upperBound,ap)
+                                      case ~(ap,None)    => println(s"repe $ap"); ap}
+    | "("~>seq~")"~opt(repe_range)  ^^ {case ~(~(seq,")"),Some(r)) => repe_seq(r.lowerBound,r.upperBound,seq)
+                                      case ~(~(seq,")"),None) => seq
+                                      case _ => println("what happened?"); new atom_prop_node(false.asBool)}
+  )
+  //[*m], [*m:n], [*m:$], [*], [+] 
+  def repe_range: Parser[repe_op] = 
+  (
+      "["~> tuple2 <~"]"            ^^ {t => repe_op(t._1,t._2)}    
+    | "["~"*"~> tuple2 <~"]"        ^^ {t => repe_op(t._1,t._2)}
+  )   
+  def tuple2: Parser[Tuple2[Int,Int]] =
+  (
+      wholeNumber~":"~wholeNumber   ^^ {case ~(~(m,":"),n) => Tuple2(m.toInt,n.toInt)}
+                                    // case x => throw new Exception("Illegal input!") 
+    | wholeNumber~":"~"$"           ^^ {case ~(~(m,":"),"$") => Tuple2(m.toInt, -1)}
+    | wholeNumber                   ^^ (x => Tuple2(x.toInt, x.toInt))
+    | "+"                           ^^ (x => Tuple2(1, -1))
+    | "*"                           ^^ (x => Tuple2(0, -1))
+  )
+
+  // handling reserved keyword  
+  def atom_prop: Parser[atom_prop_node] = 
+  (
+    """[a-zA-Z_]\w+""".r         ^^ {x => 
+                                        {
+                                          println(s"??? $x")
+                                          //Notice: no exception handling
+                                          val variable = o.getClass.getMethod(x).invoke(o)
+                                          atom_prop_node(variable.asInstanceOf[Bool])
+                                        }}
+    | """[a-zA-Z_&&[^XFG]]""".r  ^^ {x => 
+                                        {
+                                          println(s"??? $x")
+                                          //Notice: no exception handling
+                                          val variable = o.getClass.getMethod(x).invoke(o)
+                                          atom_prop_node(variable.asInstanceOf[Bool])
+                                        }}
+  )  
+}
+
+trait svaStmt
+case object svaAssertStmt extends svaStmt
+case object svaAssumeStmt extends svaStmt
+
+object svaAnno
+{
+  def svaAssert(o:Object, e:Bool) =
+  {
+    val res = o.asInstanceOf[Module].reset
+    val en = !res.asBool
+    println(s"en: $en")
+    dontTouch(en)
+    val clo = o.asInstanceOf[Module].clock
+    val mod = o.asInstanceOf[Module]
+
+    val svaSeq =  Seq(atom_prop_node(e))
+    println(svaSeq)
+
+    svaSeq.foreach{
+      case a: atom_prop_node => dontTouch(a.signal) 
+      case b => 
+    }
+    annotate(new ChiselAnnotation {
+      // Conversion to FIRRTL Annotation 
+      override def toFirrtl: Annotation = 
+      {
+        val svaanotation : Seq[Seq[sva_node]] = svaSeq map {
+          case atom_prop_node(ap) => Seq(atom_prop_anno(ap.toTarget))
+          case otherOp: svaElementAnno => Seq(otherOp)
+        } 
+        println(s"svaAnnotation: ${svaanotation.toSeq}")
+        new svaAssertAnno(svaanotation:+Seq(ResetAnno(res.toTarget)):+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
+        // new svaAnno(svaanotation:+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
+      }
+    })
+  }
+
+  def svaAssert(o:Object, s:String) =
+  {
+    val res = o.asInstanceOf[Module].reset
+    val en = !res.asBool
+    println(s"en: $en")
+    dontTouch(en)
+    val clo = o.asInstanceOf[Module].clock
+    dontTouch(clo)
+    val mod = o.asInstanceOf[Module]
+    val svaTree = new sva_tree(o)
+    println(svaTree.prop6)
+    val syntaxTree = svaTree.parseAll(svaTree.prop6, s)
+    println(s"$res, $syntaxTree")
+    val svaSeq = syntaxTree.get.treeSerialize()
+    println(svaSeq)
+
+    svaSeq.foreach{
+      case a: atom_prop_node => dontTouch(a.signal) 
+      case b => 
+    }
+    annotate(new ChiselAnnotation {
+      // Conversion to FIRRTL Annotation 
+      override def toFirrtl: Annotation = 
+      {
+        val svaanotation : Seq[Seq[sva_node]] = svaSeq map {
+          case atom_prop_node(ap) => Seq(atom_prop_anno(ap.toTarget))
+          case otherOp: svaElementAnno => Seq(otherOp)
+        } 
+        println(s"svaAnnotation: ${svaanotation.toSeq}")
+        new svaAssertAnno(svaanotation:+Seq(ResetAnno(res.toTarget)):+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
+        // new svaAnno(svaanotation:+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
+      }
+    })
+  }
+
+  def svaAssume(o:Object, s:String) =     
+  {
+    val res = o.asInstanceOf[Module].reset
+    val en = !res.asBool
+    println(s"en: $en")
+    dontTouch(en)
+    val clo = o.asInstanceOf[Module].clock
+    val mod = o.asInstanceOf[Module]
+    val svaTree = new sva_tree(o)
+    println(svaTree.prop6)
+    val syntaxTree = svaTree.parseAll(svaTree.prop6, s)
+    println(s"$res, $syntaxTree")
+    val svaSeq = syntaxTree.get.treeSerialize()
+    println(svaSeq)
+
+    svaSeq.foreach{
+      case a: atom_prop_node => dontTouch(a.signal) 
+      case b => 
+    }
+    annotate(new ChiselAnnotation {
+      // Conversion to FIRRTL Annotation 
+      override def toFirrtl: Annotation = 
+      {
+        val svaanotation : Seq[Seq[sva_node]] = svaSeq map {
+          case atom_prop_node(ap) => Seq(atom_prop_anno(ap.toTarget))
+          case otherOp: svaElementAnno => Seq(otherOp)
+        } 
+        // println("svaAnnotation: $svaanotation")
+        println(s"ClockAnno: ${ClockAnno(clo.toTarget)}")
+        new svaAssumeAnno(svaanotation:+Seq(ResetAnno(res.toTarget)):+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
+        // new svaAnno(svaanotation:+Seq(ClockAnno(clo.toTarget)):+Seq(ModuleAnno(mod.toTarget)):+Seq(EnableAnno(en.toTarget)))
+      }
+    })
+  }
+  
+  def generateMap2p(seq:Seq[sva_node]) : Map[Target,String] =
+  {
+    var i:Int = 0
+    val temp = seq.collect{
+      case t: targetAnno => t.target
+      case atom_prop_anno(target) => target
+    }.distinct
+    temp.map(a => a->("p" + {i+=1; i})).toMap
+  }
+
+  def SVAAnno2PSL(s: svaAnno): Tuple4[String, Map[String,Target], Target, svaStmt] = 
+  {
+    val elementSVA = s.toElementSeq().toSeq
+    println(s"elementSVA: $elementSVA")
+    val resetAn = elementSVA.filter(_.isInstanceOf[ResetAnno])
+    // assert(resetAn.size == 1,"only allow one reset signal")
+
+    val remainSVA = elementSVA.filter(!_.isInstanceOf[targetAnno])
+    println(s"remainSVA: $remainSVA")
+    val target2p = svaAnno.generateMap2p(remainSVA)
+    val p2target = target2p.toSeq.map{case Tuple2(k,v) => Tuple2(v,k)}.toMap
+    // val seq_ =mutable.Seq(remainSVA:_*)
+    val deSeri = remainSVA(0).treeDeSerialize(remainSVA.tail)
+    println(s"deserialization: ${deSeri}")
+
+    // distinguish assert with assume, assert statement need to be negated
+    val isAssert = s.isInstanceOf[svaAssertAnno]
+    val neg = if (isAssert) "! " else ""
+    val psl = neg +"G(" + deSeri._1.asInstanceOf[svaElementAnno].toPSL(target2p) + ") "
+
+    //val psl = "!" + svaAnno.toPSL(syntaxTree,target2p)
+    println(s"psl: $psl")
+    println(s"$p2target")
+    val stmt = if (isAssert) svaAssertStmt else svaAssumeStmt
+    (psl,p2target,resetAn(0).asInstanceOf[ResetAnno].target,stmt)
+  }
+}
+
+
+
+case class svaAssumeAnno(ttargets: Seq[Seq[sva_node]]) extends svaAnno
+case class svaAssertAnno(ttargets: Seq[Seq[sva_node]]) extends svaAnno
+
+trait svaAnno extends MultiTargetAnnotation{
+  /*println(ttargets.toSeq.toString)
+  println(ttargets.map(Seq(_)).toSeq.toString)*/
+  //ttargets.filter(_.isInstanceOf[atom_prop_anno])
+  val ttargets: Seq[Seq[sva_node]]
+  def copy(ts: Seq[Seq[sva_node]]) = {
+    this match {
+      case svaAssertAnno(ttargets) => svaAssertAnno(ts)
+      case svaAssumeAnno(ttargets) => svaAssumeAnno(ts)
+    } 
+  }
+
+  override val targets: Seq[Seq[Target]] = ttargets.filter(_.isInstanceOf[atom_prop_anno]).map(t => Seq(t.asInstanceOf[atom_prop_anno].signal))
+  //override duplication, but don't use it!
+  //override def update(renames: RenameMap): Seq[Annotation] = Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))
+  //def duplicate(n: T): Annotation
+  override def duplicate(n: Seq[Seq[Target]]): Annotation =  
+  { 
+    val tt:Seq[Target] = n.flatten 
+    this.copy(Seq(Seq(constantTrue()))) 
+  }
+  
+  //Seq(duplicate(targets.map(ts => ts.flatMap(renames(_)))))
+  override def update(renames: RenameMap) :Seq[Annotation]= 
+  {
+    Seq(this.copy(ttargets.map(
+      ts => ts.flatMap
+      {
+        case atom_prop_anno(target) => renames(target).map{x => atom_prop_anno(x.asInstanceOf[ReferenceTarget])}
+        case ResetAnno(target) => renames(target).map{ResetAnno(_)}
+        case ClockAnno(target) => {println(s"clock update: ${ClockAnno(target)}, ${renames(target).map{ClockAnno(_)}} "); renames(target).map{ClockAnno(_)}}
+        case EnableAnno(target) => renames(target).map{EnableAnno(_)}
+        case ModuleAnno(target) => renames(target).map{ModuleAnno(_)}
+        case a => Seq(a)
+      }
+  )))
+  }
+
+  private def crossJoin[T](list: Seq[Seq[T]]): Seq[Seq[T]] =
+    list match {
+      case Nil      => Nil
+      case x :: Nil => x.map(Seq(_))
+      case x :: xs =>
+        val xsJoin = crossJoin(xs)
+        for {
+          i <- x
+          j <- xsJoin
+        } yield {
+          Seq(i) ++ j
+        }
+    }
+  override def flat(): AnnotationSeq = crossJoin(ttargets).map(r => this.copy(r.map(Seq(_))))
+  
+  def toElementSeq(): Seq[sva_node] = ttargets.flatMap(_.slice(0,1))
+}
+
+
+
+case class target2ExprAnno(getMap: Map[Target,Expression]) extends NoTargetAnnotation
+
diff --git a/src/main/scala/chiseltest/roll/ParserHOA.java b/src/main/scala/chiseltest/roll/ParserHOA.java
new file mode 100644
index 0000000..e69de29
diff --git a/src/test/scala/chiseltest/formal/FormalBackendOption.scala b/src/test/scala/chiseltest/formal/FormalBackendOption.scala
index a740641..350c165 100644
--- a/src/test/scala/chiseltest/formal/FormalBackendOption.scala
+++ b/src/test/scala/chiseltest/formal/FormalBackendOption.scala
@@ -15,6 +15,7 @@ object FormalBackendOption {
       case Some("z3") => Z3EngineAnnotation
       case Some("cvc4") => CVC4EngineAnnotation
       case Some("btormc") => BtormcEngineAnnotation
+      case Some("pono") => PonoEngineAnnotation
       case Some("yices2") => throw new RuntimeException("yices is not supported yet")
       case Some("boolector") => throw new RuntimeException("boolector is not supported yet")
       case Some("bitwuzla") => BitwuzlaEngineAnnotation
