package ftse.formalism.arith

import java.text.DecimalFormat
import java.text.DecimalFormatSymbols
import java.util.Locale

import ArithExpr_metamodel._

/**
 * Provides implicitly a method to convert an arithmetic expression to its textual representation
 */
trait ArithExprSerializerImpl {
  implicit def transform(elem: ArithExpr) = new ArithExprSerializer(elem)
}

/**
 * Wrapper class that provides a method to convert an arithmetic expression to its textual representation
 * 
 * @param elem An arithmetic expression   
 */
class ArithExprSerializer(elem: ArithExpr) extends ArithExpr2Text {

  /**
   * Method to convert the arithmetic expression to its textual representation 
   * 
   * @return Textual representation of the arithmetic expression 
   */
  def toText: String = toText(elem)
}

/**
 * Provides a method to convert an arithmetic expression to its textual representation  
 */
trait ArithExpr2Text {
  
  /**
   * Method to convert an arithmetic expression to its textual representation 
   * 
   * @param elem An arithmetic expression
   * @return Textual representation of the arithmetic expression 
   */
  def toText(elem: ArithExpr): String = toText(elem, 0)
  
  /**
   * Recursive implementation to convert an arithmetic expression to its textual representation
   */
  protected def toText(elem: ArithExpr, depth: Int): String = elem match {
    case ArithNegate(el) => "(-" + toText(el) + ")"
    case ArithPlus(lhs, rhs) => "(" + toText(lhs) + "+" + toText(rhs) + ")"
    case ArithMinus(lhs, rhs) => "(" + toText(lhs) + "-" + toText(rhs) + ")"
    case ArithMult(lhs, rhs) => "(" + toText(lhs) + "*" + toText(rhs) + ")"
    case ArithDiv(lhs, rhs) => "(" + toText(lhs) + "/" + toText(rhs) + ")"
    case ArithExp(m, exp) => "(" + toText(m) + "^" + toText(exp) + ")"
    case ArithAtomIdentifier(id) => id
    case ArithAtomValue(value) => value match { case Left(i) => i.toString; case Right(f) => (f : String)}

    case _ => ""
  }
  
  /**
   * Implicit function to convert a double to a string with a certain resolution 
   */
  implicit def convDouble(r: Double) : String = {
    import java.util.Locale; import java.text._;
    val nf = new DecimalFormat("###0.00######################", new DecimalFormatSymbols(Locale.US)); nf.format(r)
  }
}