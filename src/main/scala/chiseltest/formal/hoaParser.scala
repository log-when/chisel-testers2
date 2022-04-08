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

class hoaParser extends HOAConsumer{
  
    // the state which index is stateNum is a default dead state to help deterministic... 
    var stateNum: Int = 0
    var initState: Int = 0
    var transitionFunc: mutable.Map[Int, mutable.Map[BooleanExpression[AtomLabel], mutable.Set[Integer]]] = mutable.Map[Int, mutable.Map[BooleanExpression[AtomLabel], mutable.Set[Integer]]]()
    var int2Ap: mutable.Map[Int,String] = mutable.Map[Int,String]()

    // var extraDeadState = 0
    var apNum: Int = 0
    // var auxVarNum: Int = 0
    
    //this process should be after parsing
    def partialDeterministic(): Unit = 
    {
        transitionFunc += stateNum -> mutable.Map(new BooleanExpression(true) -> mutable.Set(stateNum))
        for(i <- 0 until stateNum)
        {
            var tempExprs: mutable.Set[BooleanExpression[AtomLabel]] = mutable.Set[BooleanExpression[AtomLabel]]()
            var emptyStateTrans: mutable.Map[BooleanExpression[AtomLabel], mutable.Set[Integer]] =  mutable.Map[BooleanExpression[AtomLabel], mutable.Set[Integer]]()
            var trans: mutable.Map[BooleanExpression[AtomLabel], mutable.Set[Integer]] = transitionFunc.getOrElse(i,emptyStateTrans).clone()

            if(trans.isEmpty)
                throw new UnsupportedOperationException("There exists an illegal state")

            var updatedEdge: mutable.Map[BooleanExpression[AtomLabel], mutable.Set[Integer]] = mutable.Map[BooleanExpression[AtomLabel], mutable.Set[Integer]]()

            for((k,v) <- trans)
            {
                if(tempExprs.isEmpty)
                {
                    tempExprs += k
                    updatedEdge += (k -> v)
                } 
                else
                {
                    var tempExprsCopy = tempExprs.clone()
                    for(e <- tempExprsCopy)
                    {
                        if(!(e.and(k).isFALSE()))
                        {
                            tempExprs += e.and(k)
                            updatedEdge += (e.and(k) -> (updatedEdge(e) ++ v))

                            if(!(e.and(k.not()).isFALSE()))
                            {
                                tempExprs += e.and(k.not())
                                updatedEdge += (e.and(k.not()) -> updatedEdge(e))
                            }
                            if(!(k.and(e.not()).isFALSE))
                            {
                                tempExprs += k.and(e.not())
                                updatedEdge += (k.and(e.not()) -> v)
                            }
                            
                            tempExprs.remove(e)
                            updatedEdge = updatedEdge-(e)                           
                        }
                    }
                }
            }

            var isFull = tempExprs.fold(new BooleanExpression(false))((e1,e2)=>e1.or(e2))

            //println(isTrue)
            if(!isFull.isTRUE())
                updatedEdge += (isFull.not() ->  mutable.Set(stateNum))
            transitionFunc(i) = updatedEdge            
        }
    }



    @throws(classOf[HOAConsumerException])
    override def setNumberOfStates(numberOfStates: Int): Unit =
    {
        stateNum = numberOfStates
    }

    @throws(classOf[HOAConsumerException])
    override def addEdgeWithLabel(stateId: Int, labelExpr: BooleanExpression[AtomLabel], conjSuccessors: java.util.List[Integer], accSignature: java.util.List[Integer]): Unit =
    {
        if(conjSuccessors.size() != 1)
            throw new UnsupportedOperationException("Successor conjunction is not allowed")
        assert(labelExpr != null)

        var targetId:Integer = conjSuccessors.get(0)
        if(labelExpr.getAtom() != null && labelExpr.getAtom().isAlias() )
        {
            println("Nothing to do right now!")
        }
        else
        {
            if(!transitionFunc.contains(stateId))
                transitionFunc += stateId -> mutable.Map(labelExpr -> mutable.Set(targetId))
            else if(!transitionFunc(stateId).contains(labelExpr))
                transitionFunc(stateId) += (labelExpr -> mutable.Set(targetId))
            else 
                transitionFunc(stateId)(labelExpr) ++= mutable.Set(targetId)
        }
    }

    @throws(classOf[HOAConsumerException])
    override def setAPs(aps: java.util.List[String]): Unit = 
    {
        int2Ap = mutable.Map() ++ aps.asScala.zipWithIndex.map(a => (a._2, a._1)).toMap
        apNum = aps.size()
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
    override def addState(x$1: Int,x$2: String,x$3: jhoafparser.ast.BooleanExpression[jhoafparser.ast.AtomLabel],x$4: java.util.List[Integer]): Unit = {}

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
