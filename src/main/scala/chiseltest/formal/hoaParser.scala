package chiseltest.formal

import jhoafparser.ast.AtomAcceptance
import jhoafparser.ast.AtomLabel
import jhoafparser.ast.BooleanExpression
import jhoafparser.consumer.HOAConsumer
import jhoafparser.consumer.HOAConsumerException
import jhoafparser.parser.HOAFParser
import jhoafparser.parser.generated.ParseException
import scala.collection.mutable
import java.util.stream.IntStream
import collection.JavaConverters._
import scala.math._
import net.sf.javabdd.BDD


class hoaParser extends HOAConsumer{
  
    var bdd:BDDManager = new BDDManager()

    // the state which index is stateNum is a default dead state to help deterministic... 
    var stateNum: Int = 0
    var stateBits: Int = 0
    var initState: Int = 0
    var accStates: Seq[Int] = Seq()

    var transitionFunc: mutable.Map[Int, mutable.Map[BDD, mutable.Set[Integer]]] = mutable.Map[Int, mutable.Map[BDD, mutable.Set[Integer]]]()
    var int2Ap: mutable.Map[Int,String] = mutable.Map[Int,String]()
    var int2Aux: mutable.Map[Int,String] = mutable.Map[Int,String]()
    
    var apNum: Int = 0
    var auxVarNum: Int = 0
    
    def int2Bdd(i: Int, auxVars: Seq[Int]): BDD =
    {
        val bin = i.toBinaryString.reverse
        val auxVars_ : Seq[BDD] = auxVars.zipWithIndex.collect{
            case Tuple2(a:Int,b:Int) => 
            {
                /*println(a,b)
                println(bin.length())*/
                if(b < bin.length() && bin(b) == '1')
                    bdd.ithVar(a)    
                else
                    (bdd.ithVar(a)).not()
            }
        }
        //auxVarNum = if(auxVars_. > auxVarNum) {auxVars_} else() {auxVarNum}
        
        if(auxVars_.size == 1)
            {auxVars_(0)}
        else
        {
            val auxVars__ = auxVars_.slice(1,auxVars_.size)
            auxVars__.foldLeft(auxVars_(0))((a,b)=> a.and(b))
        }
    }

    def addAuxVar(): Unit =
    {
        for(i <- 0 until stateNum)
        {   
            println(s"this state: ${i}")
            var trans: mutable.Map[BDD, mutable.Set[Integer]] = transitionFunc(i)
            val neededAuxVar:Int = ceil(log(trans.size)).toInt
            // add AuxVar when there are more than 1 outgoing-edge
            if(neededAuxVar > 0)
            {
                if(neededAuxVar > auxVarNum)
                {
                    bdd.extVarNum(neededAuxVar - auxVarNum)
                    auxVarNum = neededAuxVar   
                }
                val varSeq = (apNum until apNum + neededAuxVar).toSeq
                val trans_ = mutable.Map() ++ trans.keys.zipWithIndex.collect{
                    case Tuple2(a:BDD, b:Int) =>
                        println(s"map[BDD,Int] $a, $b")
                        Tuple2(a.and(int2Bdd(b,varSeq)),trans.get(a).get)
                }.toMap
                transitionFunc(i)  = trans_
                println(s"new trans: ${trans_}")  
            }
        }
    }

    //check all accStates are badState
    def badAccs(): Boolean = {
        val accISBad = accStates.map{
            i:Int =>
            {
                // println(transitionFunc(i).size == 1)
                // println(transitionFunc(i).last._1.isOne())
                // println(transitionFunc(i).last._2)
                // println(i)
                transitionFunc(i).size == 1 & transitionFunc(i).last._1.isOne() &
                transitionFunc(i).last._2.size == 1 & transitionFunc(i).last._2.head == i
            }
        }
        println(s"accStates: $accStates")
        println(accISBad.toSeq)
        accISBad.foldLeft(true)((a,b)=> a & b)
        // false
    }

    // def isDeterministic(): Boolean = {

    // }

    //- old_addAuxVar is collated with old_partialDeterministic
    def old_addAuxVar(): Unit =
    {
        /*val ran:Seq[Int] = (2 until 6).toSeq
        val testAbove = int2Bdd(13,ran)
        println(testAbove)*/
        for(i <- 0 until stateNum)
        {
            var trans: mutable.Map[BDD, mutable.Set[Integer]] = transitionFunc(i)
            var trans_ = trans.clone()
            //var usedAuxVar: Int = 0
            for((k,v) <- trans)
            {
                if(v.size > 1)
                {
                  //  println(s"need aux: $k")
                    trans_.remove(k)
                    val neededAuxVar:Int = ceil(log(v.size)).toInt
                    if(neededAuxVar > auxVarNum)
                    {
                        bdd.extVarNum(neededAuxVar - auxVarNum)
                        auxVarNum = neededAuxVar   
                    }
                    /*println(s"v_size: ${ceil(log(v.size))}")
                    println(s"neededAuxVar: ${neededAuxVar}")*/
                    val varSeq = (apNum until apNum + neededAuxVar).toSeq
                    trans_ ++= mutable.Map() ++ v.zipWithIndex.collect{
                        case Tuple2(a:Integer, b:Int) =>
                            Tuple2(k.and(int2Bdd(b,varSeq)),mutable.Set(a))
                    }.toMap
                  //  println(s"need aux: $trans_")
                }
            }
            transitionFunc(i)  = trans_
        }
    }

    //this process should be after parsing
    //the complexity is high and highly related to the output automata of spot
    def old_partialDeterministic(): Boolean = 
    {
        var originalDeter = true
        for(i <- 0 until stateNum)
        {
            var mutualBdds: mutable.Set[BDD] = mutable.Set[BDD]()
            var trans: mutable.Map[BDD, mutable.Set[Integer]] = transitionFunc(i)

            if(trans.isEmpty)
                throw new UnsupportedOperationException("There exists an illegal state")

            var updatedEdge: mutable.Map[BDD, mutable.Set[Integer]] = mutable.Map[BDD, mutable.Set[Integer]]()

            for((k,v) <- trans)
            {
                if(mutualBdds.isEmpty)
                {
                    mutualBdds += k
                    updatedEdge += (k -> v)
                } 
                else
                {
                    var mutualBddsCopy = mutualBdds.clone()
                    var isExclusive = true
                    for(e <- mutualBddsCopy)
                    {
                        if(!(e.and(k).isZero()))
                        {
                            originalDeter = false
                            val oldEdge = updatedEdge(e)
                            
                            mutualBdds.remove(e)
                            updatedEdge = updatedEdge-(e)

                            isExclusive = false
                            mutualBdds += e.and(k)
                            updatedEdge += (e.and(k) -> (oldEdge ++ v))

                            if(!(e.and(k.not()).isZero()))
                            {
                                mutualBdds += e.and(k.not())
                                updatedEdge += (e.and(k.not()) -> oldEdge)
                            }
                            if(!(k.and(e.not()).isZero))
                            {
                                mutualBdds += k.and(e.not())
                                updatedEdge += (k.and(e.not()) -> v)
                            }                      
                        }
                    }
                    if(isExclusive)
                    {
                        mutualBdds += k
                        updatedEdge += (k -> v)
                    }
                }
            }
          //  println(s"i=$i: $mutualBdds")
            var isFull = mutualBdds.fold(bdd.zero())((e1,e2)=>e1.or(e2))

            //println(isTrue)
            if(!isFull.isOne())
                updatedEdge += (isFull.not() ->  mutable.Set(stateNum))
            transitionFunc(i) = updatedEdge            
        }
        transitionFunc += stateNum -> mutable.Map(bdd.one() -> mutable.Set(stateNum))
        originalDeter
    }

    @throws(classOf[HOAConsumerException])
    override def setNumberOfStates(numberOfStates: Int): Unit =
    {
        stateNum = numberOfStates
        
        stateBits = ceil(log(numberOfStates.toDouble + 1) / log(2)).toInt
      //  println(s"log(numberOfStates.toDouble + 1): ${log(numberOfStates.toDouble + 1)}")
      //  println(s"numberOfStates: $numberOfStates, stateBits: $stateBits")
    }

    @throws(classOf[HOAConsumerException])
    override def addEdgeWithLabel(stateId: Int, labelExpr: BooleanExpression[AtomLabel], conjSuccessors: java.util.List[Integer], accSignature: java.util.List[Integer]): Unit =
    {
        if(conjSuccessors.size() != 1)
            throw new UnsupportedOperationException("Successor conjunction is not allowed")
        assert(labelExpr != null)

        var targetId:Integer = conjSuccessors.get(0)
        if(labelExpr.getAtom() != null && labelExpr.getAtom().isAlias() )
            throw new UnsupportedOperationException("Unsupported label booleanExpression")
        else
        {
            val labelBdd = bdd.boolExpr2Bdd(labelExpr)
            if(!transitionFunc.contains(stateId))
                transitionFunc += stateId -> mutable.Map(labelBdd -> mutable.Set(targetId))
            else if(!transitionFunc(stateId).contains(labelBdd))
                transitionFunc(stateId) += (labelBdd -> mutable.Set(targetId))
            else 
                transitionFunc(stateId)(labelBdd) ++= mutable.Set(targetId)
        }
    }

    @throws(classOf[HOAConsumerException])
    override def setAPs(aps: java.util.List[String]): Unit = 
    {
        int2Ap = mutable.Map() ++ aps.asScala.zipWithIndex.map(a => (a._2, a._1)).toMap
        apNum = aps.size()
        if(apNum == 0)
            throw new UnsupportedOperationException("BA is empty, this assertion is meaningless")
        bdd.setVarNum(apNum)
    }

    @throws(classOf[HOAConsumerException])
    override def addStartStates(stateConjunction: java.util.List[Integer]): Unit = 
    {
        if(stateConjunction.size() != 1)
            throw new UnsupportedOperationException("only allow one initial state")
        initState = stateConjunction.get(0)
    }

    @throws(classOf[HOAConsumerException])
    override def addAlias(name: String, labelExpr: BooleanExpression[AtomLabel]): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def addEdgeImplicit(x: Int, x2: java.util.List[Integer], x3: java.util.List[Integer]): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def addMiscHeader(x$1: String,x$2: java.util.List[Object]): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def addProperties(x$1: java.util.List[String]): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def addState(id: Int, info: String, labelExpression: jhoafparser.ast.BooleanExpression[jhoafparser.ast.AtomLabel], accSignature: java.util.List[Integer]): Unit = 
    {
        if(accSignature != null && accSignature.size() > 0)
        {
            accStates :+= id
        }
    }

    @throws(classOf[HOAConsumerException])
    override def parserResolvesAliases(): Boolean = {
        false
    }

    @throws(classOf[HOAConsumerException])
    override def provideAcceptanceName(x$1: String,x$2: java.util.List[Object]): Unit = {}
   
    @throws(classOf[HOAConsumerException])
    override def setAcceptanceCondition(x$1: Int,x$2: jhoafparser.ast.BooleanExpression[jhoafparser.ast.AtomAcceptance]): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def setName(x$1: String): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def setTool(x$1: String,x$2: String): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def notifyHeaderStart(version: String): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def notifyAbort(): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def notifyBodyStart(): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def notifyEnd(): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def notifyEndOfState(stateId: Int): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def notifyWarning(warning: String): Unit = {}

}
