/*
 * These are the parser combinators for a Boolean Formula
 * @author  Martin Riedl (c) 2009
 */
package ftse.formalism.logical

import scala.util.parsing.combinator._

/*
 * Boolean Formula Parser Combinator
 */
trait LogicExpr_Parser extends JavaTokenParsers {

  import LE_metamodel._
  
  def defineParserOR: Parser[String] = {
    "|" | "OR"
  }

  def defineParserAND: Parser[String] = {
    "&" | "AND"
  }

  def defineParserNOT: Parser[String] = {
    "not" | "!"
  }

  def defineParserBrackets: (Parser[String], Parser[String]) = {
    ("(", ")")
  }

  def B_Expr_PC: Parser[Option[LogicalExpr]] =
    rep1sep(B_Term_PC, defineParserOR) ^^ { // TODO: ALL ELEMENTS
    lst =>
      try {
        lst.reduceLeft((a, b) => Some(Disjunct(a.get, b.get)))
      } catch {
        case e: Exception => throw new Exception(lst.toString); None
      }
    }

  def B_Term_PC: Parser[Option[LogicalExpr]] = {
    rep1sep(B_NOT_FAC_PC, defineParserAND) ^^ { // TODO: ALL ELEMENTS
    lst =>
      try lst.reduceLeft((a, b) => Some(Conjunct(a.get, b.get))) catch {
        case e: Exception => throw new Exception(lst.toString); None
      }
    }
  }

  def B_NOT_FAC_PC: Parser[Option[LogicalExpr]] =
    opt(defineParserNOT) ~ B_FAC_PC ^^ {
      case Some(_) ~ Some(le) => Some(Negate(le));
      case None ~ Some(le) => Some(le);
      case sthelse => assert(false, "B_NOT_FAC parsing failed"); None
    }

  def B_FAC_PC: Parser[Option[LogicalExpr]] =
    defineParserBrackets._1 ~> B_Expr_PC <~ defineParserBrackets._2 ^^ {
      case Some(le) => Some(le)
      case sthelse => throw new Exception("B_NOT_FAC parsing failed " + sthelse); None
    } |
      B_ATOM_PC ^^ {
        case Some(atom) => Some(atom)
        case sthelse => throw new Exception("B_FAC parsing failed " + sthelse); None
      }

  def B_ATOM_PC: Parser[Option[LogicalExpr]] //= ident ^^ {id => Some(AtomStr(id))}  
}
