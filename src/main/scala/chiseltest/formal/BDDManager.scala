package chiseltest.formal

import jhoafparser.ast.Atom
import jhoafparser.ast.AtomLabel
import jhoafparser.ast.BooleanExpression
import jhoafparser.ast.BooleanExpression.Type
import net.sf.javabdd.BDD
import net.sf.javabdd.BDDFactory
import net.sf.javabdd.BDDPairing

class BDDManager(numnNodes:Int = 125000, numCache:Int = 100000, numInc: Int = 10000) {

    var LIB:String = "javabdd"
    var bdd : BDDFactory = BDDFactory.init(LIB,numnNodes,numCache)
    

    bdd.setMaxIncrease(numInc)

    def zero():BDD = bdd.zero()
    def one():BDD = bdd.one()
    //def extVarNum(num:Int):Unit = bdd.extVarNum(num)

    def ithVar(varr:Int):BDD =
    {
        assert(bdd != null)
        bdd.ithVar(varr)
    }
    def setVarNum(num:Int):Unit = 
    {
        assert(bdd != null)
        bdd.setVarNum(num)
    }

    def extVarNum(num:Int):Unit = 
    {
        assert(bdd != null)
        bdd.extVarNum(num)        
    }

    @throws(classOf[Exception])
    def boolExpr2Bdd(boolExpr:BooleanExpression[AtomLabel]): BDD =
    {
        assert(boolExpr != null)
        if(boolExpr.isTRUE())
            bdd.one()
        else if(boolExpr.isFALSE())
            bdd.zero()
        else if(boolExpr.isAtom())
            bdd.ithVar(boolExpr.getAtom().getAPIndex())
        else if(boolExpr.isNOT())
            boolExpr2Bdd(boolExpr.getLeft()).not()
        else if(boolExpr.isAND())
            boolExpr2Bdd(boolExpr.getLeft()).and(boolExpr2Bdd(boolExpr.getRight()))
        else if(boolExpr.isOR())
            boolExpr2Bdd(boolExpr.getLeft()).or(boolExpr2Bdd(boolExpr.getRight()))
        else
            throw new UnsupportedOperationException("Unsupported BooleanExpression structure")
    }

}
