package ftse.transformations.tra

import ftse.formalism.tra._
import ftse.transformations._

//TODO: (Martin -> Alex) check whether the comment here is correct in the sense that it is an IMMEDIATE and not a DISCRETE transition (with a probability given) 
/**
 * This trait implements the AbstTranformer trait in order to follow the stackable trait pattern.
 * It is used to transform the Markovian transitions into immediate transitions.
 * Therefore it uses embedded Markov chains (it calculates per state the sum of all outgoing
 * rates and builds the quotient "rate/sum" for each transition.
 */
trait TraEmbeddedMarkovChain extends TraTransformer {

  /**
   * This method performs the real transformation. The user is advised to eliminate vanishing states before (!)
   * applying this method.
   * 
   * @param tra A transition system 
   * @return //TODO  
   */
  abstract override def transform(tra: Tra) : Tra = {
    
    // provide result structure
    var newTransitions = Map[Long, Iterable[Transition]]();
    
    // for every state: take each outgoing transition into account
	for (state <- tra.states) {
	  
	  // calculate sum of all values
	  val sum = tra.transitions.getOrElse(state,List[Transition]()).foldLeft(0.0)((old, transition) => old + transition.value);
	  
	  // generate new transitions
	  var newTransitionsOfState = List[Transition]();
	  
	  val transitions = tra.transitions.get(state);
	  transitions match {
	    
	    // no transitions available -> generate one single transition, that stays in this state
	    case None => newTransitionsOfState = newTransitionsOfState :+ ImmediateTransition(state,"generatedTransition",state,1);
	    
	    // if transitions are available -> calculate new values
	    case Some(iterable) => {
	      newTransitionsOfState = iterable.foldLeft(List[Transition]()) ( (old, transition) => 
	    						old :+ ImmediateTransition(transition.source, transition.label,
	    						    				transition.target, (transition.value / sum)));
	    }
	  }  
	  
	  // save transitions in map
	  newTransitions += (state -> newTransitionsOfState);
	}
	
	// generate new Tra-object
	val newTra = Tra(newTransitions, tra.states) 
    
	// perform other transformations
	super.transform(newTra)
  }  
}