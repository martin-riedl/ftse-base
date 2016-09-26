package ftse.formalism.arith

import scala.util.parsing.combinator._

import ArithExpr_metamodel._
import ftse.tools.Helper._

/**
 * Arithmetic Expression Parser (for ArithExpr)
 */
trait ArithExpr_Parser extends JavaTokenParsers {

  def ArithExpr_PC: Parser[ArithExpr] =
    (ArithTerm_PC ~ ("+" | "-") ~ ArithTerm_PC) ^^ {
      case lhs ~ "+" ~ rhs => ArithPlus(lhs, rhs)
      case lhs ~ "-" ~ rhs => ArithMinus(lhs, rhs)
    } |
      ArithTerm_PC

  def ArithTerm_PC: Parser[ArithExpr] =
    (ArithFactor_PC ~ ("*" | "/" | "^" | "%") ~ ArithFactor_PC) ^^ {
      case lhs ~ "%" ~ rhs => ArithMod(lhs, rhs)  
      case lhs ~ "*" ~ rhs => ArithMult(lhs, rhs)
      case lhs ~ "/" ~ rhs => ArithDiv(lhs, rhs)
      case lhs ~ "^" ~ rhs => ArithExp(lhs, rhs)
    } | ArithFactor_PC

  def ArithFactor_PC: Parser[ArithExpr] =
    "(" ~> ArithExpr_PC <~ ")" |
      ident ^^ { id => ArithAtomIdentifier(id) } |
      floatingPointNumber ^^ { 
        case Int(value) => ArithAtomValue(Left(value))
        case value => ArithAtomValue(Right(value.toDouble)) 
      } /*|
      decimalNumber ^^ { 
        value => ArithAtomValue(Left(value.toInt)) 
      } */
}




/**
 * Arithmetic Expression Parser for AE[T]
 */
abstract trait AE_Parser[T] extends JavaTokenParsers {
  
  def AE_PC: Parser[AE[T]] = (AETerm_PC ~ ("+" | "-") ~ AETerm_PC) ^^ {
    case l ~ "+" ~ r => AE_Plus[T](l,r)
    case l ~ "-" ~ r => AE_Min[T](l,r)
  } | AETerm_PC
  
  private def AETerm_PC : Parser[AE[T]] = (AEFactor_PC ~ ("*" | "/" | "^" | "%") ~ AEFactor_PC) ^^ {
    case l ~ "%" ~ r => AE_Mod(l,r)
    case l ~ "*" ~ r => AE_Mult(l,r)
    case l ~ "/" ~ r => AE_Div(l,r)
    case l ~ "^" ~ r => AE_Exp(l,r)
  } | AEFactor_PC
  
  def AEFactor_PC : Parser[AE[T]] =  "(" ~> AE_PC <~ ")" | AEAtom_PC
  def AEAtom_PC : Parser[AE[T]] 
}

/*
trait AE_RefParser[T] extends AE_Parser[T] {
  abstract override def AEAtom_PC : Parser[AE[T]] = ident ^^ {
    case bla => AE_Reference[T](bla)
  } | super.AEAtom_PC
}

trait AE_ValueParser[T] extends AE_Parser[T] {
  abstract override def AEAtom_PC : Parser[AE[T]] = decimalNumber ^^ {
    case bla => AE_Value[T](bla.toInt) 
  } | super.AEAtom_PC
}
*/

/*
trait Combined extends AE_Valued[Combined] with AE_Refs[Combined]


object t {
  
  val parser = new AE_ValueParser[Combined] with AE_RefParser[Combined]
  
  val pResult = parser.parseAll(parser.AE_PC,"")
  
  val value = AE_Value[Combined](1)
  val ref = AE_Reference[Combined]("bla")
  val stuff : AE[Combined] = AE_Neg(AE_Plus(value,ref))
  
  
}*/

/*
trait AE_Parser[+T<:AE[T]] extends JavaTokenParsers {
  def AE_PC: Parser[T] = /*(AETerm_PC ~ ("+" | "-") ~ AETerm_PC) |*/ AETerm_PC
  
  private def AETerm_PC[U >: T <:AE[T]] : Parser[U] = (AEFactor_PC ~ ("*" | "/" | "^") ~ AEFactor_PC) ^^ {
     case lhs ~ "*" ~ rhs => AE_Mult[T](lhs, rhs) 
     //case lhs ~ "/" ~ rhs => AE_Div[T](lhs, rhs) : T
     //case lhs ~ "^" ~ rhs => AE_Exp[T](lhs, rhs) : T
  } //| AEFactor_PC[T]
  
  def AEFactor_PC[U >: T <:AE[T]] : Parser[U] =  "(" ~> AE_PC <~ ")" | AEAtom_PC
  def AEAtom_PC : Parser[T]
  */
  /*
  def AE_PC: Parser[T] = {
    (AETerm_PC ~ ("+" | "-") ~ AETerm_PC) ^^ {
      case lhs ~ "+" ~ rhs => AE_Plus[T](lhs, rhs) 
      case lhs ~ "-" ~ rhs => AE_Minus[T](lhs, rhs)
    } //| AETerm_PC
  }
  
  private def AETerm_PC[T<:AE[T]] : Parser[T] = {
    (AEFactor_PC ~ ("*" | "/" | "^") ~ AEFactor_PC) ^^ {
      case lhs ~ "*" ~ rhs => AE_Mult[T](lhs, rhs)
      case lhs ~ "/" ~ rhs => AE_Div[T](lhs, rhs)
      case lhs ~ "^" ~ rhs => AE_Exp[T](lhs, rhs)
    } | AEFactor_PC
  }
  

  private def AEFactor_PC[T<:AE[T]]: Parser[T] =  "(" ~> AE_PC <~ ")" | AEAtom_PC
    
  def AEAtom_PC[T<:AE[T]] : Parser[T]
 
} 

trait AE_Parser_Ext[+T<:AE_Ext] extends AE_Parser[AE_Ext] {
  def AEAtom_PC : Parser[AE[AE_Ext]] = 
    AE_Value_PC 
    
  def AE_Value_PC : Parser[AE[AE_Ext]] = floatingPointNumber ^^ { 
        case Int(value) => {AE_Value(Left(value))}
        case value => {AE_Value(Right(value.toDouble))}
      } |
      decimalNumber ^^ { 
        value => AE_Value(Left(value.toInt)) 
      }
      
  def AE_Identifier_PC : Parser[AE[AE_Ext]] = 
    ident ^^ { id => (AE_Identifier(id)) }
}
*/