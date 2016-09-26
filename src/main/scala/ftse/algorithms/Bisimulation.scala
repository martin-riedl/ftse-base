package ftse.algorithms

import ftse.formalism.tra._
import scala.collection.immutable.HashMap

/**
 * Encapsulates methods to check bisimulation equivalency on transition systems
 */
object Bisimulation {
  import ftse.formalism.tra.Tra
 
  /**
   * Returns the action label from a given transition
   * 
   * @param t a transition
   * @return the action label
   */
  def action(t : Transition) : String = t.label
  
  /**
   * Returns the assigned value of a given transition
   * 
   * @param t a transition
   * @return the corresponding float value
   */
  def value(t : Transition) : Float = ("%.6f" format t.value).replace(",",".") toFloat // to assure a conversion e.g. 0.4999999 => 0.5 otherwise one obtains problems with partition identifications
  
  /**
   * A class C is split into Cr1,Cr2,...Crk, iff the cumulative rates of the transitions within C with destination into Csplit are determined to be r1,r2,...rk
   * 
   * see M. Siegle and H. Hermanns - Bisimulation Algorithms for Stochastic Process Algebras and their BDD-Implementation
   * y(P,a,C):= \sum_{y\in E(P,a,C)}\lambda\,\,, where\,\, E(P,a,C):= \{\lambda| \,\, P\longrightarrow^{a,\lambda}P'\wedge P'\in C\}
   *  
   */   
  def markovianStrongBisimulation(transitions: Map[(Int, Long),Iterable[ftse.formalism.tra.Transition]], P : (Int, Long), a : String, C : Set[(Int,Long)]) = 
    (transitions(P) filter (t => C contains ((P._1,t.target))) map (value(_))).foldLeft(0.0)(_+_)
    
  /**
   * A class C is split into Cr1,Cr2,...Crk, iff for a certain label $a$ and cumulative rates of the transitions within C corresponding to $a$ with destination into Csplit are determined to be r1,r2,...rk
   *     
   * extension to [[Bisimulation#markovianStrongBisimulation]]
   */
  def markovianActionLabelledStrongBisimulation(transitions: Map[(Int, Long),Iterable[ftse.formalism.tra.Transition]], P : (Int, Long), a : String, C : Set[(Int,Long)]) = 
    (transitions(P) filter (t => (C contains ((P._1,t.target))) & (t.label==a)) map (value(_))).foldLeft(0.0)(_+_)

  /**
   * A class C is split into Cp,Cm iff for a certain label $a$ the transitions of the states within C corresponding to $a$ with destination into Csplit are determined  
   */
  def strongBisimulation(transitions: Map[(Int, Long),Iterable[ftse.formalism.tra.Transition]], P : (Int, Long), a : String, C : Set[(Int,Long)]) = {
    !(transitions(P) filter (t=> (C contains ((P._1,t.target)))  & (t.label==a)) isEmpty)
  }
  
  /**
   * Performs an equivalence check on the given transition systems.
   * 
   * @param equivNotion the splitting criteria 
   * @param tras an array of transition systems 
   * @return true if the transitions systems behave equivalent (i.e. if the all start states are in the same equivalence class) considering the splitting criteria or else false
   */
  def checkEquivalence(equivNotion : (Map[(Int, Long),Iterable[ftse.formalism.tra.Transition]], (Int, Long), String, Set[(Int,Long)]) => Any,tras: Tra*) : Boolean = {
    val enumTras = tras zipWithIndex
    val transitions = Map( enumTras flatMap(t => t._1.states map(s => (t._2,s) -> t._1.transitions.getOrElse(s,List()) )) :_*) 
    val S = Set(enumTras flatMap(t => t._1.states map (s => (t._2,s)))  :_*)
    val Partitions = scala.collection.mutable.Set[Set[(Int, Long)]](S)
    val Act = transitions.flatMap(a => a._2 map (_.label)) 
    val Splitters = scala.collection.mutable.Set((for (a <- Act; p <- Partitions) yield (a, p)).toList: _*)
    
    /**
     * Splitting function
     * 
     * @param C the class that is split into Cr1,Cr2,...Crk corresponding to the equivNotion 
     * @param a the action label
     * @param the destination class of the transitions starting in C 
     * @return a tuple that contains the new partitions, new splitters, the old partition and the old splitters
     */
    def split(
      C: scala.collection.immutable.Set[(Int, Long)],
      a: String,
      Cspl: Set[(Int, Long)]) = {
      val split_tree = (C map (P => (equivNotion(transitions,P,a,Cspl) -> P))) groupBy (_._1) map(entry => entry._1 -> entry._2.map(_._2))
      if (split_tree.size>1) { 
         val newPartitions = split_tree.values
         val newSplitters  = for (a<-Act;p<-split_tree.values) yield (a,p)
         val oldPartition = List(C)
         val oldSplitters = (for (a<-Act) yield (a,C)) 
         (newPartitions, newSplitters, oldPartition, oldSplitters)
      }
      else (List(),List(),List(),List())
    }
    
    while (!Splitters.isEmpty) {
      val splitter = Splitters.head; val a = splitter._1; val Cspl = splitter._2
      
      for (C <- Partitions) {
        val (newPartitions, newSplitters, oldPartition, oldSplitters) = split(C, a, Cspl)

		Partitions ++= newPartitions
        Partitions --= oldPartition
        
        Splitters ++= newSplitters
        Splitters --= oldSplitters 
      }
      
      Splitters -= splitter  
    }
    
    val areTrasEquiv = Partitions.foldLeft(false)((b,p) => if (b==true) true else {				//	iterate through all partitions
      if (p contains (0,1)) {																	//		if in one of the partitions the start state is found
        enumTras.foldLeft(true)((b,e)=> if (b==true && (p contains (e._2,1))) true else false)  //			all other states also have to be there	
      } else false																				//		else not TS is not equivalent
    })																							
    
    areTrasEquiv
  }
  
}
