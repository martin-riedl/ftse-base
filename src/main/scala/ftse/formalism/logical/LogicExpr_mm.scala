/*
 * This internal domain specific language represents a Boolean Formula. Furthermore a parser combinator is defined.
 * @author  Martin Riedl (c) 2009
 */
package ftse.formalism.logical

import scala.collection.immutable._

/**
 * Common boolean expression interface 
 */
trait LogicalExpr { 
  import LE_metamodel._
  /**
   * infix conjunction with another logical expression 
   * @param right Right-hand-side logical expression 
   * @return [[LogicalExpr]]
   */
  def &(right: LogicalExpr): LogicalExpr = right match {
    case BooleanAtom(true) => this
    case BooleanAtom(false) => BooleanAtom(false)
    case NeutralAtom => this
    case _ => Conjunct(this, right)
  }
  
  /**
   * infix disjunction with another logical expression 
   * @param right Right-hand-side logical expression 
   * @return [[LogicalExpr]]
   */
  def |(right: LogicalExpr): LogicalExpr = right match {
    case BooleanAtom(true) => this
    case BooleanAtom(false) => this
    case NeutralAtom => this
    case _ => Disjunct(this, right)
  }
}


object LE_metamodel {
	/** Common boolean expression leaf element */
	trait LExpAtom extends LogicalExpr 
	
	/** Common interface of an unary operator on logical expressions */
	abstract class UnaryOp(arg: LogicalExpr) extends LogicalExpr
	/** Common interface of an binary operator on logical expressions */
	abstract class BinaryOp(left: LogicalExpr, right: LogicalExpr) extends LogicalExpr
	
	/**
	 * Unary operator Negate 
	 * 
	 * @param arg A logical expression to negate
	 * @return [[LogicalExpr]]
	 */
	sealed case class Negate(arg: LogicalExpr) extends UnaryOp(arg)
	
	/**
	 * Infix operator to conjunct two logical expressions 
	 * 
	 * @param left The left-hand-side logical expression 
	 * @param right The right-hand-side logical expression
	 * @return [[LogicalExpr]]
	 */
	sealed case class Conjunct(left: LogicalExpr, right: LogicalExpr) extends BinaryOp(left, right)
	
	/**
	 * Infix operator to disjunct two logical expressions 
	 * 
	 * @param left The left-hand-side logical expression 
	 * @param right The right-hand-side logical expression
	 * @return [[LogicalExpr]]
	 */
	sealed case class Disjunct(left: LogicalExpr, right: LogicalExpr) extends BinaryOp(left, right)
	
	/**
	 * Boolean leaf element
	 * @param b Boolean value  
	 */
	case class BooleanAtom(b : Boolean) extends LExpAtom {
	  /**
	   * infix operator to conjunct with a logical expression
	   * @param right The logical expression to conjunct with   
	   */
	  override def &(right: LogicalExpr): LogicalExpr = if (this.b) right else this
	  
	  /**
	   * infix operator to disjunct with a logical expression
	   * @param right The logical expression to disjunct with   
	   */
	  override def |(right: LogicalExpr): LogicalExpr = right
	}
	
	/** Neutral element concerning an arbitrary operation on a logical expression */
	case object NeutralAtom extends LExpAtom {
	  /**
	   * the infix operator to conjunct with another logical expression 
	   * 
	   * @param the logical expression to conjunct with 
	   * @return the logical expression to conjunct with  
	   */
	  override def &(right: LogicalExpr): LogicalExpr = right
	
	  /**
	   * the infix operator to disjunct with another logical expression 
	   * 
	   * @param the logical expression to disjunct with 
	   * @return the logical expression to disjunct with  
	   */
	  override def |(right: LogicalExpr): LogicalExpr = right
	}
	
	/**
	 * Should not be used anymore
	 */
	@deprecated case class AtomState(ci: Iterable[String], state: String) extends LExpAtom
	

}
