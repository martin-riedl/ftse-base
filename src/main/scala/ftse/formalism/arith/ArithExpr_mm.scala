package ftse.formalism.arith

import scala.util.parsing.combinator._



/**
 * Arithmetic Expression Metamodel
 */
object ArithExpr_metamodel {
	
	/** Common abstract arithmetic expression element */
	abstract class ArithExpr 
	
	/** Common abstract unary arithmetic operator element */ 
	abstract class UnaryArithOp(val arg: ArithExpr) extends ArithExpr
	
	/** Common abstract binary arithmetic operator element */ 
	abstract class BinaryArithOp(val left: ArithExpr, val right: ArithExpr) extends ArithExpr
	
	/** Enables pattern matching on abstract class AE_BinaryOp */ 
	object BinaryArithOp {
	  def unapply(bo : BinaryArithOp) = Some((bo.left ,bo.right))
	}
	
	/** Enables pattern matching on abstract class AE_UnaryOp */
	object UnaryArithOp {
	  def unapply(uo : UnaryArithOp) = Some((uo.arg))
	}
	
	/** Common abstract Leaf Element */
	abstract class ArithAtom extends ArithExpr
	
	/** Negation Operator */
	sealed case class ArithNegate(override val arg: ArithExpr) extends UnaryArithOp(arg)
	
	/** Addition Operator */
	sealed case class ArithPlus(override val left: ArithExpr, override val right: ArithExpr) extends BinaryArithOp(left, right)
	
	/** Multiplication Operator */
	sealed case class ArithMult(override val left: ArithExpr, override val right: ArithExpr) extends BinaryArithOp(left, right)
	
	/** Modulo Operator */
	sealed case class ArithMod(override val left: ArithExpr, override val right: ArithExpr) extends BinaryArithOp(left, right)
	
	/** Subtraction Operator */
	sealed case class ArithMinus(override val left: ArithExpr, override val right: ArithExpr) extends BinaryArithOp(left, right)
	
	/** Division Operator */
	sealed case class ArithDiv(override val left: ArithExpr, override val right: ArithExpr) extends BinaryArithOp(left, right)
	
	/** Exponential Operator */
	sealed case class ArithExp(override val left: ArithExpr, override val right: ArithExpr) extends BinaryArithOp(left, right)
	
	/** Leaf Element with Label*/
	sealed case class ArithAtomIdentifier(id: String) extends ArithAtom
	
	/** Leaf Element with Value */
	sealed case class ArithAtomValue(value: Either[Int, Double]) extends ArithAtom
	
	
	
	
	
	
	/**
	 * AE[T] defines a common abstract arithmetic expression element using type parameters, i.e. abstract types (e.g. AE_Base)
	 * 
	 * @tparam T An arbitrary Type Parameter to distinguish different kinds of arithmetic expressions  
	 */
	trait AE[T]
	
	/** Common abstract Leaf Element */
	trait AE_Atom[T] extends AE[T]
	
	/** Common abstract unary arithmetic operator element */ 
	abstract class AE_UnaryOp[T](val t : AE[T]) extends AE[T]
	
	/** Common abstract binary arithmetic operator element */ 
	abstract class AE_BinaryOp[T](val l : AE[T], val r : AE[T]) extends AE[T]
	
	
	/** Enables pattern matching on abstract class AE_BinaryOp with parameter extraction */ 
	object AE_BinaryOp {
	  def unapply[T](bo : AE_BinaryOp[T]) = Some((bo.l ,bo.r))
	}
	
	/** Enables pattern matching on abstract class AE_UnaryOp with parameter extraction */
	object AE_UnaryOp {
	  def unapply[T](uo : AE_UnaryOp[T]) = Some((uo.t))
	}
	
	/** Addition Operator */
	case class AE_Plus[T](override val l:AE[T], override val r:AE[T]) extends AE_BinaryOp[T](l,r)
	
	/** Minimum Operator */
	case class AE_Min[T](override val l:AE[T], override val r:AE[T]) extends AE_BinaryOp[T](l,r)
	
	/** Multiplication Operator */
	case class AE_Mult[T](override val l:AE[T], override val r:AE[T]) extends AE_BinaryOp[T](l,r)
	
	/** Modulo Operator */
	case class AE_Mod[T](override val l : AE[T], override val r: AE[T]) extends AE_BinaryOp[T](l,r)
	
	/** Negation Operator */
	case class AE_Neg[T](override val t:AE[T]) extends AE_UnaryOp[T](t)
	
	/** Common abstract extension element */
	trait AE_Extended[T] extends AE[T]
	
	/** Exponential Operator */
	case class AE_Exp[T](override val l:AE[T],override val r:AE[T]) extends AE_BinaryOp[T](l,r) with AE_Extended[T]
	
	/** Division Operator */
	case class AE_Div[T](override val l:AE[T],override val r:AE[T]) extends AE_BinaryOp[T](l,r) with AE_Extended[T] 
	
	/** Leaf Element with Value */
	trait AE_Valued[T] extends AE[T]
	case class AE_Value[T](i : Double) extends AE_Valued[T] with AE_Atom[T]
	
	/** Leaf Element with Label */
	trait AE_Refs[T] extends AE[T] 
	case class AE_Reference[T](s : String) extends AE_Refs[T] with AE_Atom[T]  
}
