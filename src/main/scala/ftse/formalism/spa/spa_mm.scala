/*
 * This is an internal domain specific interface for building stochastic process algebra object trees. 
 * @author  Martin Riedl (c) 2009
*/

package ftse.formalism.spa

import ftse.formalism.arith._
import ArithExpr_metamodel._

import ftse.formalism.logical._
import LE_metamodel._

/**
 * CASPA Stochastic Process Algebra (SPA) Metamodel
 */
object SPA_metamodel {
	/**
	 * common SPA element class 
	 */
	abstract class SPA_Element // extends BaseElement
	
	/**
	 * SPA specification
	 * 
	 * @param defs Contains all definitions of the SPA specification    
	 */
	case class SPASpec(defs: Iterable[Definition]) extends SPA_Element
	
	/**
	 * Instantiation Parameter
	 * 
	 * @param int 
	 */
	case class ProcessParameter(init: Int) extends SPA_Element
	
	/**
	 * Process Parameter Definition 
	 * @param name 
	 * @param max 
	 */
	case class PPD(name: String, max: Either[Int,String]) extends SPA_Element 
	
	/**
	 * Process Impl Definition
	 * @param guard
	 * @param p 
	 */
	
	case class GuardedProcess(guard: Guard, p: Process) extends Process 
	
	/**
	 * An SPA Guard
	 * 
	 * @param conds A list of SPA conditions
	 */
	case class Guard(conds : List[AbstrCondition]) extends SPA_Element
	
	/**
	 * Defines an enumeration of Operators that can be used inside SPA Conditions
	 * 
	 * provides the following operators: !=,==,<=,>=,<,>
	 */
	object ConditionOp extends Enumeration/*("!=","=","<=",">=","<",">")*/ {
	      type ConditionOp = Value
	      val !=,==,<=,>=,<,> = Value
	    }
	
	import ConditionOp._
	
	case class SMDefAtom(process : ProcRef, optInt : Option[Int]) extends LExpAtom
	case class ProcRef(variable : String, conditions : Option[List[AbstrCondition]]) extends SPA_Element
	
	/**
	 * common condition element 
	 */
	abstract class AbstrCondition extends SPA_Element
	
	/**
	 * A specific condition consisting of two operands and an infix operation 
	 * 
	 * @param lhs An arithmetic expression 
	 * @param rhs An arithmetic expression 
	 */
	case class Condition(lhs : ArithExpr, op : ConditionOp, rhs : ArithExpr) extends AbstrCondition // TODO: check if this simple form does probably not capture the complete SPA language
	
	/**
	 * Is always true independent of the parameters 
	 */
	case object AllCondition extends AbstrCondition 
	
	/**
	 * common SPA Definition element  
	 */
	abstract class Definition extends SPA_Element
	
	/**
	 * A CASPA SPA process definition 
	 * 
	 * @param name A process name 
	 * @param parameters A sequence process parameters 
	 * @param gps A set of guarded processes 
	 * @return [[Definition]]
	 */
	sealed case class ProcessDefinition(name: String, parameters: Seq[PPD], gps: Iterable[Process]) extends Definition // Process Intf Definition
	
	/**
	 * A CASPA SPA constant definition 
	 * 
	 * @param name Constant name
	 * @param arithexpr An arithmetic expression
	 */
	sealed case class ConstantDefinition(name: String, arithexpr: ArithExpr) extends Definition
	
	abstract class BindDefinition extends Definition
	case class RateBinding(name: String, value: Either[Double,Int]) extends BindDefinition 
	case class WeightBinding(name: String, value: Either[Double,Int]) extends BindDefinition 
	
	/**
	 * The stop-process
	 */
	case object Stop extends Process
	
	/**
	 * Wrapper to represent a set of process definitions also as a valid SPA element 
	 */
	case class ProcessDefinitions(pds: Iterable[ProcessDefinition]) extends SPA_Element // wrapper class
	
	/**
	 * Process Instantiation
	 * 
	 * @param variable Variable name
	 * @param args Sequence of arithmetic expressions  
	 */
	case class I(variable: String, args: Seq[ArithExpr]) extends Process
	
	/**
	 * CASPA parallel composition infix operator
	 * 
	 * A left-hand-side and a right-hand-side process are composed and synchronized 
	 * via some action labels denoted by a specific synchronization set. 
	 * 
	 * @param p1 Left-hand-side process   
	 * @param p2 Right-hand-side process 
	 * @param actionlabels Synchronization set
	 * @return [[Process]]
	 */
	case class ParallelComposition(p1: Process, p2: Process, actionlabels: Iterable[String]) extends Process
	
	/**
	 * CASPA choice infix operator
	 * 
	 * @param ps1 Left-hand-side process
	 * @param ps2 Right-hand-side process
	 * @return [[Process]]
	 */
	case class Choice(ps1: Process, ps2: Process) extends Process
	
	/**
	 * CASPA hide prefix operator
	 * 
	 * A set of action labels denote which actions shall be hidden, i.e. represented as tau-actions, inside a given process.   
	 * 
	 * @param action A set of action labels to be hidden  
	 * @param process The process on which the hiding should be applied
	 * @return [[Process]]
	 */
	case class Hiding(action: Set[String], process: Process) extends Process
	
	/**
	 * CASPA prefix operator 
	 * 
	 * Defines a sequential process by prefixing a certain action before another behavior follows. 
	 * 
	 * @param a The prefixed action
	 * @param fb The followed process
	 * @return [[Process]]
	 */
	case class Prefix(a: Action, fb: Process) extends Process
	
	
	/**
	 * Markovian action 
	 * 
	 * Markovian action following an exponential distribution 
	 * 
	 * @param label Action label 
	 * @param r Rate of the exponential distribution
	 * @return [[Action]]  
	 */
	case class MA(override val label: String, r: ArithExpr) extends Action(label)
	
	/**
	 * Immediate weighted action  
	 * 
	 * @param label Action label 
	 * @param w Weight
	 * @return [[Action]]  
	 */
	case class IA(override val label: String, w: ArithExpr) extends Action(label)
	
	/**
	 * Common labeled action element
	 * 
	 * @param label Action label
	 * @return SPA_Element
	 */
	abstract class Action(val label: String) extends SPA_Element
	
	/**
	 * Common process element 
	 */
	abstract class Process extends SPA_Element
	
	/**
	 * Common measure definition
	 * 
	 * @param variable The measure variable name
	 * @return [[Definition]] 
	 */
	abstract class MeasureDefinition(variable : String) extends Definition
	
	/**
	 * State measure definition
	 * @param variable See [[MeasureDefinition]]
	 * @param sm A logical expression specifying the set of composed states satisfying the expression 
	 * @return [[MeasureDefinition]]
	 */
	case class StateMeasure(variable : String, sm : LogicalExpr) extends MeasureDefinition(variable)
	
	/**
	 * Mean measure definition
	 * @param variable See [[MeasureDefinition]]
	 * @param refProcess
	 * @param refParameter
	 * @param optNumber 
	 * @return [[MeasureDefinition]]
	 */
	
	case class MeanValue(variable : String, refProcess : String, refParameter : String, optNumber : Option[Int]) extends MeasureDefinition(variable)
	
	/**
	 * Throughput measure definition
	 * @param variable See [[MeasureDefinition]]
	 * @param actionLabel The action to be considered for the throughput measure 
	 * @return [[MeasureDefinition]]
	 */
	case class ThroughputMeasure(variable : String, actionLabel : String) extends MeasureDefinition(variable)
}

/**
 * Internal DSL (Domain Specific Language) for the CASPA SPA denoted by some methods and Implicits 
 */
trait SPA {
  import SPA_metamodel._
  
  /** wrapper class for implicit construction of a process definition */
  class ProcessDefinitionImplicit(name: String, parameters: Seq[PPD]) {
	  def :=(p: Iterable[Process]) = ProcessDefinition(name, parameters, p)
  }
  
  /** wrapper class for implicit construction of a parallel composition */
  class openSynchParallel(p1: Process, actionlabels: Iterable[String]) {
	  def |(p2: Process) = ParallelComposition(p1, p2, actionlabels)
  }
  
  /** wrapper class for implicit construction of composition */
  class ProcessImplicit(lhs : Process) {
	  def ||(rhs: Process) = ParallelComposition(lhs, rhs, List()) // Interleaving
	  def |(actionlabels: Iterable[String]) = new openSynchParallel(lhs, actionlabels)
	  def +(rhs: Process) = Choice(lhs, rhs)    
  }
  
  /**
   *  wrapper class for implicit construction of a sequential process by prefixing  
   */
  class ActionImplicit(action : Action) {
    def /(fb: Process) = Prefix(action, fb)
  }
 
  /**
   * constructs a SPA Guard with a wildcard condition  
   */
  def Gall = {
    Guard(List(AllCondition))
  }

  /**
   * constructs a SPA Guard with a equivalence check of the given parameters
   * 
   * @param s A process parameter  
   * @param i Some value
   */
  def G(s: String, i : Int) = {
    val condition = C(s,"=",i)
    Guard(List(condition))
  }
  
  /**
   * constructs a SPA Guard with a boolean relation of the given parameters
   * 
   * @param s A process parameter
   * @param op Boolean relation operator  
   * @param i Some value
   */  
  def G(s: String, op : String, i : Int) = {
    val condition = C(s,op,i)
    Guard(List(condition))
  }
  
  def C(s: String, op : String, i : Int) = {
    Condition(ArithAtomIdentifier(s), ConditionOp.withName(op), ArithAtomValue(Left(i)))
  }
  
  def G(l : List[Condition]) = {
    Guard(l)
  }
  
  

  
  def I(ref: String, init: Int) : I = SPA_metamodel.I(ref, List(ArithAtomValue(Left(init))))
  def I(ref: String, init : String) : I = SPA_metamodel.I(ref, List(ArithAtomIdentifier(init)))
  def I(ref: String) :I  = SPA_metamodel.I(ref, List())
  
  def MA(label: String, r: Double) = SPA_metamodel.MA(label,ArithAtomValue(Right(r)))
  def IA(label: String, r: Double) = SPA_metamodel.IA(label,ArithAtomValue(Right(r)))
  
  def MA(label: String, r: String) = {
    try {
      SPA_metamodel.MA(label,ArithAtomValue(Right(r.toDouble)))
    } catch {
      case sth => throw sth //ftse.formalism.spa.MA(label,ArithAtomIdentifier(r))
    }
  }
  
  def IA(label: String, r: String) = {
    try {
      SPA_metamodel.IA(label,ArithAtomValue(Right(r.toDouble)))
    } catch {
      case sth => throw sth //ftse.formalism.spa.IA(label,ArithAtomIdentifier(r))
    }
  }
  
  def hide(al: Set[String], p: Process) = Hiding(al, p)

  /* implicit conversions to simplify the internal dsl */
  implicit def impl_Process(p : Process) = new ProcessImplicit(p)
  implicit def impl_Action(a : Action) = new ActionImplicit(a)
  implicit def impl_Specification(l: Iterable[Definition]) = SPASpec(l)
  implicit def impl_ProcDef(pd: (String, Seq[PPD])) = new ProcessDefinitionImplicit(pd._1, pd._2)
  implicit def impl_Processe(mp: (Guard, Process)) = GuardedProcess(mp._1, mp._2)
  implicit def impl_ParamDef(p: (String, Int)) = PPD(p._1, Left(p._2))
}
