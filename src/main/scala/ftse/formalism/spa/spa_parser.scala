package ftse.formalism.spa


import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._

import scala.util.parsing.input.CharArrayReader.EofCh


import ftse.formalism.spa._

import ftse.formalism.arith._
import ftse.formalism.logical._

import ftse.tools.Helper._

trait SPA_Parser extends JavaTokenParsers with ArithExpr_Parser with LogicExpr_Parser with SPA {
  import SPA_metamodel._
  
  def SPASpec_PC: Parser[SPASpec] = 
    rep(
        ProcessDefinition_PC | 
        Bind_PC | 
        Def_PC | 
        Measure_PC | 
        "/*" ~> rep( elem("chrExcept", ch => (ch != EofCh) && ch != '/' && ch !='*' ) | '/' ~ not('*') | '*' ~ not('/')) <~"*/" | 
        "//" ~> rep( elem("chrExcept", ch => (ch!= EofCh) && ch != '\n' ) )
    ) ^^ {
    	case lines => {
    	  SPASpec(lines collect {case d : Definition => d})
    	}
  	}
    
  
  def B_ATOM_PC: Parser[Option[LogicalExpr]] = {
    ProcRef_PC ~ opt("{" ~> decimalNumber <~ "}") ^^ {
      case process ~ optNumber =>  Some(SMDefAtom(process, optNumber.map(a=> a.toInt)))
    }
  }
  
  def ProcRef_PC : Parser[ProcRef] = {
    ident ~ opt("("~> repsep(Condition_PC,",") <~")") ^^ {
      case variable ~ optConditions => {
        ProcRef(variable,optConditions)
      }
    } |
    ident <~ "("~ "*" ~")" ^^ {
      case variable => {
        ProcRef(variable,Some(List()))
      }
    }
    
  }
  
  def Measure_PC : Parser[MeasureDefinition] = 
    (ThroughputMeasure_PC | StateMeasure_PC |  MeanValue_PC) ^^ { case m => m }
 
  def ThroughputMeasure_PC : Parser[MeasureDefinition] = 
    "throughputmeasure" ~ ident ~ ident ^^ {case _ ~ name ~ action => ThroughputMeasure(name,action)}
    
  def MeanValue_PC : Parser[MeanValue] = 
    "meanvalue" ~ ident ~ ident ~ ("(" ~> ident <~ ")") ~ opt("{" ~> decimalNumber <~ "}") ^^{
      case _ ~ name ~ procRef ~ paramRef ~ optNumber => 
        MeanValue(name,procRef,paramRef,optNumber.map(a=> a.toInt)) 
    }
  
  
  def StateMeasure_PC : Parser[StateMeasure] = 
    "statemeasure" ~> ident ~ B_Expr_PC ^^ {
    	case variable ~ Some(smdef : LogicalExpr) => StateMeasure(variable, smdef) 
    }
  
    
  def Bind_PC : Parser[BindDefinition] = 
    ("rate" | "weight") ~ ident ~ "=" ~ floatingPointNumber ~ ";" ^^ {
    	case "rate" ~ id ~ _ ~ Int(i)  ~ _=> RateBinding(id,Right(i))
    	case "weight" ~ id ~ _ ~ Int(i)  ~ _=> WeightBinding(id,Right(i))    	
    	case "rate" ~ id ~ _ ~ f  ~ _=> RateBinding(id,Left(f.toFloat))
    	case "weight" ~ id ~ _ ~ f  ~ _=> WeightBinding(id,Left(f.toFloat))    	      
    }
  
  def Def_PC : Parser[ConstantDefinition] = 
    ("int" ~> ident <~ "=") ~ ArithExpr_PC ~ ";" ^^ {
    	case id ~ ae ~ _ => ConstantDefinition(id,ae)
    }
    
  def ProcessDefinition_PC : Parser[Definition] = {
    ident ~ ("(" ~> Parameters_PC <~ ")") ~ ":=" ~ rep(GuardedProcess_PC) ^^ {
      case name ~ parameters ~ ":=" ~ processes => ProcessDefinition(name,parameters, processes)
    } | ident ~ ":=" ~ Process_PC ^^ {
      case name ~ ":=" ~ process => ProcessDefinition(name,List(), List(process))
    } 
  }
  
  def Parameters_PC : Parser[List[PPD]] = {
    rep1sep(Parameter_PC,",") ^^ {
      case params => params
    }   
  }
  
  def Parameter_PC : Parser[PPD] = {
    ident ~ ("[" ~> decimalNumber <~ "]") ^^ {
      case id ~ max =>  PPD(id, Left(max.toInt))
    } | 
    ident ~ ("[" ~> ident <~ "]") ^^ {
      case id ~ max => PPD(id,Right(max))
    }   
  }
  
  def GuardedProcess_PC : Parser[GuardedProcess] = {
    (Guard_PC <~ "->") ~ SequentialProcess_PC ^^ {
      case guard ~ p => GuardedProcess(guard,p)
    }
  }
  
  def Guard_PC : Parser[Guard] = {
		  "[" ~> (  
		      rep1sep(Condition_PC,",")  ^^ {
		      	case conditions => Guard(conditions)
		      } | 
		      "*" ^^ {
		      	case _ => Guard(List())
		      }
		   ) <~ "]"
  }
  
  def Condition_PC : Parser[AbstrCondition] = { 
    val tmp = ConditionOp.values.map(a => a.toString)
    val alternativeOperators = tmp.tail.foldLeft[Parser[String]](tmp.head)(_ | _)
    
    ArithExpr_PC ~ (alternativeOperators) ~ ArithExpr_PC ^^ {
    	case lhs ~ op ~ rhs => Condition(lhs,ConditionOp.withName(op),rhs)
  	}
  }
  
  def Process_PC : Parser[Process] = {
	  (("hide" ~> rep1sep(ident,",") <~ "in") ~ Process_PC)  ~ opt(Process1_PC) ^^ { 
	    case actions ~ p ~ Some(next) => Hiding(actions.toSet,p) | next._1 | next._2 
	    case actions ~ p ~ None => Hiding(actions.toSet,p)  
	  }  |
	  /*b*/  SequentialProcess_PC ~ opt(Process1_PC) ^^ { 
	    case sp ~ Some(next) => sp | next._1 | next._2 
	    case sp ~ None => sp
	  }|
	  /*b*/  ("(" ~> Process_PC <~ ")") ~ opt(Process1_PC) ^^ { 
	    case p ~ Some(next) => p | next._1 | next._2 
	    case p ~ None => p  
	  } 
  }
  
  def Process1_PC : Parser[(List[String],Process)] = {
    ("|[" ~> repsep(ident,",") <~ "]|") ~ Process_PC ^^ { 
      case actions ~ p2 =>(actions,p2)
    }
  }
  
  def SequentialProcess_PC  : Parser[Process] = { // weakest binding, i.e. choice 
    SequentialProcess1_PC ~ "+" ~ SequentialProcess1_PC ^^ {
      case p1 ~ _ ~ p2 => p1 + p2
    } | SequentialProcess1_PC
  }

  def SequentialProcess1_PC  : Parser[Process] = { // stronger binding i.e. action prefix 
    rep1sep(("(" ~ "*") ~> ident ~ "," ~ ArithExpr_PC <~ ("*" ~")"), ";") ~ ";" ~ SequentialProcess_PC ^^ {
      case actions ~ _ ~ p => actions.foldRight(p)((action, process) => SPA_metamodel.IA(action._1._1, action._2)/process)   
    } |
    rep1sep("(" ~> ident ~ "," ~ ArithExpr_PC <~ ")", ";") ~ ";" ~ SequentialProcess_PC ^^ { 
      case actions ~ _ ~ p => actions.foldRight(p)((action, process) => SPA_metamodel.MA(action._1._1, action._2)/process)   
    } |
    SequentialProcess2_PC
  }  
  
  def SequentialProcess2_PC  : Parser[Process] = {
    "(" ~> SequentialProcess_PC <~ ")" ^^ {case p => p} | 
    "stop" ^^ {case _ => Stop} |
    ident ~ opt("(" ~> rep1sep(ArithExpr_PC,",") <~ ")") ^^ { 
      case id ~ Some(args) => SPA_metamodel.I(id,args.toSeq)
      case id ~ None => I(id)
    }
  }
}
