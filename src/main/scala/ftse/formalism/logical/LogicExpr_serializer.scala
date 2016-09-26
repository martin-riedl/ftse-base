package ftse.formalism.logical

/**
 * Boolean Expression Serializer 
 */
trait LogicExpr2Text {
  import LE_metamodel._
  val not = "not"
  /**
   * Serializes a logical expression
   * @param elem A logical expression 
   * @return The string representation of a logical expression  
   */  
  def toText(elem: LogicalExpr): String = elem match {
    case a: LExpAtom => toText(a)
    case Conjunct(lhs, rhs) => "(" + toText(lhs) + " & " + toText(rhs) + ")"
    case Disjunct(lhs, rhs) => "(" + toText(lhs) + " | " + toText(rhs) + ")"
    case Negate(elem) => "(" + not + " " + toText(elem) + ")"
  }

  /**
   * Serializes a logical expression leaf element 
   * 
   * @param elem A logical expression leaf element
   * @return The string representation of the leaf element
   */
  def toText(elem: LExpAtom): String = elem match {
    //case AtomStr(value) => value
    case AtomState(ns, name) => ns.mkString("", "_", "_") + name
  }
}
