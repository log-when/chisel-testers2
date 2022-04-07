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
  
    var stateNum: Int = 0
    var transitionFunc: mutable.Map[Int, mutable.Map[BooleanExpression[AtomLabel], Integer]] = mutable.Map[Int, mutable.Map[BooleanExpression[AtomLabel], Integer]]()
    var apmap: mutable.Map[Int,String] = mutable.Map[Int,String]()

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
                transitionFunc += stateId -> mutable.Map(labelExpr -> targetId)
            else
                transitionFunc(stateId) += (labelExpr -> targetId)
        }
    }

    @throws(classOf[HOAConsumerException])
    override def setAPs(aps: java.util.List[String]): Unit = 
    {
        var ap:mutable.Map[Int,String] = mutable.Map() ++ aps.asScala.zipWithIndex.map(a => (a._2, a._1)).toMap
    }

    @throws(classOf[HOAConsumerException])
    override def addAlias(name: String, labelExpr: BooleanExpression[AtomLabel]): Unit = 
    {

    }

    @throws(classOf[HOAConsumerException])
    override def addEdgeImplicit(x: Int, x2: java.util.List[Integer], x3: java.util.List[Integer]): Unit = 
    {

    }

    @throws(classOf[HOAConsumerException])
    override def addMiscHeader(x$1: String,x$2: java.util.List[Object]): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def addProperties(x$1: java.util.List[String]): Unit = {}

    @throws(classOf[HOAConsumerException])
    override def addStartStates(x$1: java.util.List[Integer]): Unit = {}

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
