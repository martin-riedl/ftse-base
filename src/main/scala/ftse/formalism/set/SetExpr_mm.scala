package ftse.formalism.set

import ftse.formalism.arith._
import ArithExpr_metamodel._
/**
 * Set Expression MetaModel 
 */
object SetExpr_metamodel {
    object Comparator extends Enumeration {
    	type Comparator = Value
    	val !=, <=, ==, >= ,<,> = Value
    }
    import Comparator._

	sealed case class SetFilter(se : SetExpr, cmp : Comparator, crit : ArithExpr) extends SetExpr
	

	/** Common SetExpr element */
	abstract class SetExpr
	
	/** Common SetExpr leaf element */
	abstract class SetAtom extends SetExpr
	
	/**
	 * Union infix operator 
	 * 
	 * @param left [[SetExpr]]
	 * @param right [[SetExpr]]
	 * @return [[SetExpr]]
	 */
	sealed case class SetUnion(left: SetExpr, right: SetExpr) extends SetExpr
	
	/**
	 * Intersection infix operator 
	 * 
	 * @param left [[SetExpr]]
	 * @param right [[SetExpr]]
	 * @return [[SetExpr]]
	 */
	sealed case class SetIntersect(left: SetExpr, right: SetExpr) extends SetExpr
	
	/**
	 * Difference infix operator 
	 * 
	 * @param left [[SetExpr]]
	 * @param right [[SetExpr]]
	 * @return [[SetExpr]]
	 */
	sealed case class SetDiff(left: SetExpr, right: SetExpr) extends SetExpr
	
	/**
	 * Range leaf elements 
	 * 
	 * @param begin Starting from the evaluation of 'begin'
	 * @param end Ending with the evaluation of 'end'
	 * @return [[SetAtom]]
	 */
	sealed case class SetRange(begin: ArithExpr, end: ArithExpr) extends SetAtom
	
	/**
	 * Set leaf elements
	 * 
	 * @param elements A list or arithmetic expressions to be evaluated  
	 * @return [[SetAtom]]
	 */
	sealed case class SetBase(elements: List[ArithExpr]) extends SetAtom

}
