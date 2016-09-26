package ftse.algorithms

import scala.collection.immutable.HashMap

/**
 * Provides a sorting function to perform topological sorting
 */
trait TopologicalOrdering { 
  /**
   * sorts a list of unsorted elements of type ELEM in topological order 
   * 
   * @param unsorted the list of unsorted elements
   * @param getId maps an element of type ELEM to some identifier of type ID
   * @param dependsOn a function that denotes the dependability to other 
   * elements by mapping for each element of type ELEM the set of dependencies in terms of the identifier
   * @return the topological sorted list of elements of type ELEM  
   */
  def sort[ID,ELEM](unsorted : List[ELEM], getId : ELEM => ID, dependsOn : ELEM => Set[ID]) : List[ELEM] = {
      val idsToConsider = Set(unsorted.map(getId(_)) :_*)
      val elementMap = HashMap(unsorted.map(e=> (getId(e), e)).toList :_*)
      val dependability = HashMap((unsorted map { e =>(getId(e) -> dependsOn(e))}).toList :_*)
    
      val L = new scala.collection.mutable.ListBuffer[ID]()   // empty list that will contain the sorted nodes 
	  val S = unsorted.map(getId(_)) // set of all nodes with no incoming edge
	  
	  val visited = scala.collection.mutable.HashSet[ID]()
	  
	  S.foreach(n => visit(n))
	  
	  /**
	   * a visiting helper function (with side effect on the HashSet visited)
	   * 
	   * @param n identifier of type ID
	   */
	  def visit(n : ID) : Unit = {
	    if (!visited.contains(n) && (idsToConsider contains n) ) {
	      visited.add(n)
	      dependability(n).foreach(
	          m=> 
	            visit(m)
	      )
	      L+=(n)
	    }
	  }
      
      L.toList.map(elementMap(_)) 
  } 
}

/**
 * Provides a function to perform some kind of operation on a topological sorted list 
 */
trait OrderedProcessing {
  /**
   * function that performs an operation on each element 
   * 
   * @param topologicalsorted a list of elements of type SOURCE
   * @param process a function that processes a source element of type SOURCE 
   * to some target element of type TARGET, by providing the already processed dependencies, 
   * i.e in terms of a list of elements of type TARGET  
   * @return a list of processed elements of type TARGET 
   */
  def perform[SOURCE,TARGET](topologicalsorted: List[SOURCE], process : (SOURCE, List[TARGET]) => TARGET) : List[TARGET] = {
      performRek(topologicalsorted, List(), process)
  }
  
  /**
   * recursive function called by (@see perform) defined that the first value is generated and another call 
   * is done recursively with the remaining iterators an the already generated conditions
   */    
  private def performRek[SOURCE,TARGET](topologicalsorted: List[SOURCE], processed : List[TARGET], process : (SOURCE, List[TARGET]) => TARGET) : List[TARGET] = {
    if (topologicalsorted.isEmpty) processed else {
      val oldhead = topologicalsorted.head
      val newhead = process(oldhead, processed)
      val newprocessed = processed ::: List(newhead)
      newhead :: performRek(topologicalsorted.tail,newprocessed, process)
    }
  }
}

/**
 * Trait that incorporates both, the topological ordering and the processing, to provide a single function to process 
 * acyclic dependent elements   
 */
trait TopologicalOrderedProcessing extends TopologicalOrdering with OrderedProcessing {
  /**
   * function that sorts and processes acyclic dependent elements
   * 
   * @param unsorted (see [[TopologicalOrdering#sort]])
   * @param getId (see [[TopologicalOrdering#sort]])
   * @param dependsOn (see [[TopologicalOrdering#sort]])
   * @param process (see [[OrderedProcessing#perform]])
   * 
   * @return a list of processed elements of type TARGET 
   */
  def sortAndProcess[ID,SOURCE,TARGET](
      unsorted : List[SOURCE], 
      getId : SOURCE => ID, 
      dependsOn : SOURCE => Set[ID], 
      process : (SOURCE, List[TARGET]) => TARGET
  ) = {
     perform(sort(unsorted,getId,dependsOn), process)
  }
}

