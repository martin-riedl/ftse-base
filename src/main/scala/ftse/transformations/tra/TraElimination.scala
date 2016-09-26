package ftse.transformations.tra
import ftse.formalism.tra._

/**
 * Eliminates all vanishing states in the Tra-Object.
 */
trait TraElimination extends TraTransformer {
  
  /**
   * Eliminates vanishing states
   * 
   * @param transitions A list of transitions
   * @param states A set of states
   * @return A triple consisting of a list of new transitions, 
   * the remaining transitions i.e. those, which source or target 
   * state is a remaining state and the set of remaining states
   */
  private def eliminationStep(transitions : List[Transition], states : Set[Long]) = {
      val reachedByImmediate = transitions collect {case ImmediateTransition(_,_,target,_) => target}
      val groupedBySource = transitions groupBy(t => t.source)
	  val markovianIn = (transitions collect {case m : MarkovianTransition => m}) groupBy (t => t.target)
      
      // change: ignore the initial state
      val vanishingIOutgoing = (states -- reachedByImmediate - (1L)).toList map 
      	(s => s -> (groupedBySource.getOrElse(s,List()) collect {case i : ImmediateTransition => i})) filter (_._2.nonEmpty)
      	
      val newTransitions = vanishingIOutgoing flatMap { e => val (state, itransitions) = e
        val sum = itransitions.foldLeft(0.0)((a,b) => a + b.value)
        markovianIn(state).flatMap(m => itransitions.map(i => MarkovianTransition(m.source,m.label,i.target,i.value*m.value/sum)))
      }
	  
      val remainingStates = states -- vanishingIOutgoing.map(_._1)
     
      (newTransitions,transitions.filter(t => (remainingStates contains t.source) && (remainingStates contains t.target) ), remainingStates)
  }
  
  /**
   * Eliminates all vanishing states in the Tra-object and generates a new one
   * 
   * @param tra A transition system
   * @return An eliminated transition system
   */
  abstract override def transform(tra : Tra) : Tra = {

    var transitions = tra.transitions.values.flatten(a=>a).toList
    var states = tra.states
    var newT = List[Transition]()
    
    do {
       // initial state is the first state (number "1") as stated in ftse.transformations.ReachTransformer.reachToTra()
       val (n,remT,s) = eliminationStep(transitions,states)
       newT=n
       transitions = n ::: remT; 
       states = s
    } while (newT.nonEmpty)
    
    val newTra = Tra(transitions.groupBy(_.source),states)
    
    super.transform(newTra)
  }
}
