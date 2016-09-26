package ftse.algorithms

import ftse.formalism.tra._

/**
 * Reachability Algorithm for Transition Systems 
 */
object Reachability {
  
  /**
   * @param t A transition System
   * @return A transition system comprising only reachable states
   */
  def reachability(t : Tra) = {
    
    val init = t.states.toList.sortWith(_<_).head
    val done = scala.collection.mutable.Set[Long](init)
    val todo = scala.collection.mutable.Queue[Long](init)
    val rg = scala.collection.mutable.Map[Long, Iterable[Transition]]()
    while (todo.nonEmpty) {
      val current = todo.dequeue
      if (t.transitions contains current) {
	      val transitions = t.transitions(current)
	      for (t <- transitions) {
	        if (! (done contains t.target)) {
	          todo.enqueue(t.target)
	          done.add(t.target)
	        }
	      }
	      rg += (current -> transitions)        
      }
    }
    
    Tra(rg.toMap, done.toSet)
  }

}