/*
 * This internal domain specific language represents a transition system parsed from a CASPA TRA output
 * @author  Martin Riedl (c) 2009
 */
package ftse.formalism.tra

/**
 * Transition system 
 * 
 * @param transitions A mapping from each source state to its corresponding emanating transitions 
 * @param states All states (including those with no emanating transitions)
 */
case class Tra(transitions: Map[Long, Iterable[Transition]], states: Set[Long]) 


/**
 * A common class for a labeled transition that provides some value
 * 
 * @param source Source state
 * @param label Transition label 
 * @param target Target state
 * @param value Some value of type double  
 */
abstract class Transition(val source: Long, val label: String, val target: Long, val value: Double) 


/**
 * A Markovian transition 
 * 
 * @param source Source state
 * @param label Transition label 
 * @param target Target state
 * @param value Transition rate  
 */
final case class MarkovianTransition(override val source: Long, override val label: String, override val target: Long, 
    override val value: Double) extends Transition(source, label, target, value)


/**
 * An Immedatiate transition 
 * 
 * @param source Source state
 * @param label Transition label 
 * @param target Target state
 * @param value Transition weight  
 */
final case class ImmediateTransition(override val source: Long, override val label: String, override val target: Long, 
    override val value: Double) extends Transition(source, label, target, value)


/**
 * This class is used as an extension of the Tra objects.
 * One use case is the reward model, which is defined in ftse.formalism.tra.reward.
 * This ExtTra class should be used as an orthogonal extension.
 */
abstract class ExtTra