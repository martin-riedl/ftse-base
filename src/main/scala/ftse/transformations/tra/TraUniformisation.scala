package ftse.transformations.tra

import ftse.formalism.tra._
import ftse.transformations._

/**
  * This trait implements the AbstTranformer trait in order to follow the stackable trait pattern.
  * It is used to transform the Markovian transitions into immediate transitions.
  * Therefore it calculates a number m that is greater than the sum of all outgoing rates of each (!) state.
  * Afterwards it calculates the quotient rate/m for each transition and inserts self-loops in each state
  * with (m-sum(rates_in_this_state))/m as the outgoing rate.
  */
trait TraUniformisation extends TraTransformer {

  /**
   * This method performs the real transformation. The user is advised to eliminate vanishing states before (!)
   * applying this method.
   * 
   * @param tra A transition system
   * @return Uniformized transition system
   */
  abstract override def transform(tra: Tra) : Tra = {
    
    // at first: calculate the m value per maximum search
    var mValue = 0.0
    
    // step through every state
    for (state <- tra.states) {
      
      // calculate sum of outgoing transitions
      val outgoingSum = tra.transitions.getOrElse(state,List()).foldLeft(0.0)((old, transition) => old + transition.value)
      
      // store new value if greater than already stored value
      if (outgoingSum > mValue)
        mValue = outgoingSum
    }
    
    // now mValue holds the maximum sum of all outgoing rates
    
    // step again through every state and calculate the new transition values
    var newTransitions: Map[Long, Iterable[Transition]] = Map()
    for (state <- tra.states) {
      
      var newTransitionsOfState: List[Transition] = List()
      
      // calculate sum of outgoing transitions
      val outgoingSum = tra.transitions.getOrElse(state,List()).foldLeft(0.0)((old, transition) => old + transition.value)
      
      // take every transition into account
      for (transition <- tra.transitions.getOrElse(state, List())) {
        
        // generate new transition
        val newTransition = ImmediateTransition(transition.source, transition.label, transition.target, transition.value / mValue)
        
        // add the new transition to the list
        newTransitionsOfState = newTransitionsOfState :+ newTransition
      }
      
      // generate the self-loop-transition for this state and add it to the list
      val selfLoop = ImmediateTransition(state, "selfloop", state, (mValue-outgoingSum) / mValue)
      newTransitionsOfState = newTransitionsOfState :+ selfLoop
      
      // add an entry in the transition map
      newTransitions += ((state -> newTransitionsOfState))
    } 
    
    // generate the new tra
    val newTra = Tra(newTransitions, tra.states)
    
    // perform other transformations
	super.transform(newTra)
  }  
}