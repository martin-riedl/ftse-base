package ftse.algorithms

/**
 * Provides set theoretic functions 
 */
object SetTheory {
  /**
   * Constructs the power set on set of elements of type A
   * 
   * @param s the set of elements 
   * @return a set of sets containing all subsets of the set of elements of type A
   */
  def powerSet[A](s: Set[A]) =
    s.foldLeft(Set(Set.empty[A])) { (set, element) =>
      set union (set map (_ + element))
    }
  
  /**
   * Constructs the cross-product on all lists of elements of type A
   * 
   * @param xx the list of lists of elements of type A
   * @param f may contain a function filtering product terms while construction (only true evaluated product terms will pass)
   * @return the cross-product in terms of a list of lists of elements of type A
   */
  def xproduct[A](xx: List[List[A]], f : Option[List[A] => Boolean] = None) : List[List[A]] = xx match {
	  case aa :: Nil => f.foldLeft(aa.map(a => List(a)))((a,filt) => a.filter(filt(_)))
	  case aa :: bb => f.foldLeft(xproduct(bb,f).flatMap(li => aa.map(a=>a :: li)))((a,filt) => a.filter(filt(_)))
	  case _ => List(List())												
  }
}

