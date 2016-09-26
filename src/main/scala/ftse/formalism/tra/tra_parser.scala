package ftse.formalism.tra

import scala.util.parsing.combinator._

/**
 * Tra Parser Combinator
 */
trait TRA_Parser extends JavaTokenParsers {
  
  
  /**
   * Parses a labeled transition system
   * 
   * @param l a sequence of string containing the transition system
   * @return the transition system
   */
  def parseTra(l : Iterable[String]) : Tra = {

    val transitions = l.map(parseAll(Transition_PC,_)).filter(_.successful).map(_.get).filter(_.isDefined).map(_.get)
    val t_outgoing = transitions groupBy (t => t.source)
    val t_incoming = transitions groupBy (t => t.target)
    
    val states = t_outgoing.keySet ++ t_incoming.keySet
    
    // if no transitions have been parsed then assume 1 to be the stop state
    Tra(t_outgoing, if (states.isEmpty) Set(1) else states)
  }
  

  /**
   * Parses the a single transition of a labeled transition system 
   */
  private def Transition_PC: Parser[Option[Transition]] =
    ident ~ decimalNumber ~ decimalNumber ~ floatingPointNumber ~ opt("M"|"I")  ^^ {
      case l ~ s ~ t ~ v ~ Some("I")=> Some(ImmediateTransition(s.toLong, l, t.toLong, v.toDouble))
      case l ~ s ~ t ~ v ~ (Some("M")|None) => Some(MarkovianTransition(s.toLong, l, t.toLong, v.toDouble))
      case _ => None
    } 
}