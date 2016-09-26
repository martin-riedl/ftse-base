package ftse.transformations

import ftse.formalism.tra._

// SPA imports
import ftse.formalism.spa._
import SPA_metamodel._

/**
 * Provides an implicit method to convert a transition system to a CASPA process
 */
trait TRA2SPAImpl {
  implicit def transform(e: Tra) = new TRA_Generator(e)
}

/**
 * A wrapper class that takes a transition system as an argument to provide a method to convert the transition system to a CASPA process
 * 
 * @param elem A transition system 
 */
class TRA_Generator(elem: Tra) extends TRA2SPA {
  
  /**
   * Conversion method 
   * 
   * @param name The name of the resulting CASPA process
   * @return A CASPA process corresponding to the given name
   */
  def toSPA(name: String): SPA_Element = toSPA(name, elem)
}

/**
 * Converts a transition system to a CASPA process having a specified name 
 */
trait TRA2SPA extends SPA {
  
  /**
   * Conversion method
   * 
   * @param name The specified name of the resulting CASPA process
   * @param e A transition system
   * @return A CASPA process
   */
  def toSPA(name: String, e: Tra): SPA_Element = {
    val defs = e.transitions.foldLeft(List[ProcessDefinition]()) { (l, s) =>
      {
        val p = s._2.toList.tail.foldLeft[Process](s._2.toList.head match {
          case i: ImmediateTransition => (IA(i.label, i.value) / I(name + "_" + i.target))
          case m: MarkovianTransition => (MA(m.label, m.value) / I(name + "_" + m.target))
        })(
          (p, t) => p + (t match {
            case i: ImmediateTransition => (IA(i.label, i.value) / I(name + "_" + i.target))
            case m: MarkovianTransition => (MA(m.label, m.value) / I(name + "_" + m.target))
          }))
        ProcessDefinition(name + "_" + s._2.toList.head.source, List[PPD](), List(p)) :: l
      }
    }

    val stopstates = Set(e.states.toList: _*) -- e.transitions.keySet
    val stopprocesses = for (elem <- stopstates.toList) yield ProcessDefinition(name + "_" + elem, List[PPD](), List(Stop))

    ProcessDefinitions(defs ::: stopprocesses)
  }
}
