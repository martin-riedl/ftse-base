package ftse.formalism.set

import ftse.formalism.arith._
import scala.util.parsing.combinator._

import SetExpr_metamodel._

/**
 * Set Expression Parser
 */
trait SetExpr_parser extends JavaTokenParsers with ArithExpr_Parser {
  def SetExpr_PC: Parser[Option[SetExpr]] = SetUnion_PC

  
  def SetFilter_PC : Parser[Option[SetExpr]] = SetUnion_PC ~ ":" /*~ ("!=" | "<=" | "==" | ">=")*/ ~ ArithExpr_PC ^^ {
    case Some(set) ~ _/* ~ "!=" */~ expr => Some(SetFilter(set, Comparator.!=, expr)) 
    //case Some(set) ~ _ ~ _ ~ "==" ~ expr => Some(SetFilter(set, Comparator.==, expr))
    //case Some(set) ~ _ ~ _ ~ "<=" ~ expr => Some(SetFilter(set, Comparator.<=, expr))
    //case Some(set) ~ _ ~ _ ~ ">=" ~ expr => Some(SetFilter(set, Comparator.>=, expr))
  }

  
  def SetUnion_PC: Parser[Option[SetExpr]] = rep1sep(SetIntersect_PC, "++") ^^ { lst =>
    try lst.reduceLeft((a, b) => Some(SetUnion(a.get, b.get)))
    catch { case e: Exception => throw new Exception(lst.toString) }
  }

  def SetIntersect_PC: Parser[Option[SetExpr]] = rep1sep(SetDiff_PC, "**") ^^ { lst =>
    try lst.reduceLeft((a, b) => Some(SetUnion(a.get, b.get)))
    catch { case e: Exception => throw new Exception(lst.toString) }
  }

  def SetDiff_PC: Parser[Option[SetExpr]] = rep1sep(Intermed_PC, "--") ^^ { lst =>
    try lst.reduceLeft((a, b) => Some(SetDiff(a.get, b.get)))
    catch { case e: Exception => throw new Exception(lst.toString) }
  }
    
  def Intermed_PC: Parser[Option[SetExpr]] =
    "{" ~> SetUnion_PC ~ ":" ~ "_"  ~ ("!=" | "<=" | "==" | ">=" | ">" | "<") ~ ArithExpr_PC <~ "}" ^^ {
    	case Some(set) ~ _ ~ _ ~"!=" ~ expr => Some(SetFilter(set, Comparator.!=, expr))
    	case Some(set) ~ _ ~ _ ~"==" ~ expr => Some(SetFilter(set, Comparator.==, expr))
    	case Some(set) ~ _ ~ _ ~"<=" ~ expr => Some(SetFilter(set, Comparator.<=, expr))
    	case Some(set) ~ _ ~ _ ~">=" ~ expr => Some(SetFilter(set, Comparator.>=, expr))
    	case Some(set) ~ _ ~ _ ~"<" ~ expr => Some(SetFilter(set, Comparator.<, expr))
    	case Some(set) ~ _ ~ _ ~">" ~ expr => Some(SetFilter(set, Comparator.>, expr))
  	}|
    "(" ~> SetUnion_PC <~ ")" ^^ {
      case Some(le) => Some(le)
      case sthelse => throw new Exception("SetExpr parsing failed " + sthelse); None
    } |
      Atom_PC ^^ {
        case Some(atom) => Some(atom)
        case sthelse => throw new Exception("SetExpr parsing failed " + sthelse); None
      }
  def Atom_PC: Parser[Option[SetAtom]] =
    "{" ~ ArithExpr_PC ~ ".." ~ ArithExpr_PC ~ "}" ^^ {
      case _ ~ begin ~ _ ~ end ~ _ => Some(SetRange(begin, end))
    } |
      "{" ~ rep1sep(ArithExpr_PC, ",") ~ "}" ^^ {
        case _ ~ lst ~ _ => Some(SetBase(lst))
      }
}

