package ftse.formalism.set

import ftse.formalism.arith.ArithExprSerializerImpl

import SetExpr_metamodel._

/**
 * Set Expression Serializer
 */
trait SetExpr2Text extends ArithExprSerializerImpl {
  
  /**
   * Serializes a set expression
   * @param elem A set expression 
   * @return The string representation of the set expression  
   */
  def toText(elem: SetExpr): String = elem match {
    case SetUnion(lhs, rhs) => "(" + toText(lhs) + " ++ " + toText(rhs) + ")"
    case SetDiff(lhs, rhs) => "(" + toText(lhs) + " \\ " + toText(rhs) + ")"
    case SetIntersect(lhs, rhs) => "(" + toText(lhs) + " ** " + toText(rhs) + ")"
    case SetFilter(se, cmp, ae) => "{" + toText(se) + ":" + "_" + cmp.toString() + ae.toText + "}"
    case a: SetAtom => toText(a)
  }

  /**
   * Serializes a set expression leaf element
   * 
   * @param elem Set expression leaf element 
   * @return The string representation of the leaf element
   */
  protected def toText(elem: SetAtom): String = elem match {
    //case AtomStr(value) => value
    case SetBase(lst) => lst.map(_.toText).mkString("{", ",", "}")
    case SetRange(a, b) => "{" + a.toText + " .. " + b.toText + "}"
  }
}
