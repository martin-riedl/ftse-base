package ftse.formalism.spa

/** Process Algebra Composition Tuple */
object PACT extends SPA {
  import SPA_metamodel._
  /**
   * Synchronisation Set for PACT
   * 
   * @param consuming All actions that finally consume a synchronization 
   * @param synchaction All actions that are passively involved in a synchronized act
   * @param unsynchaction All actions that shall remain unsynchronized
   */
  case class SynchSets(
    consuming: Set[Action],
    synchaction: Set[Action],
    unsynchaction: Set[Action]
  ) {
    override def toString = consuming.map(_.label).mkString(",") + "|" + synchaction.map(_.label).mkString(",") + "|" + unsynchaction.map(_.label).mkString(",")
  }
  
  /** defines a Stop-Process */
  val empty = PAT(Stop, SynchSets(Set[Action](), Set[Action](), Set[Action]()), List[Definition]())

  /** 
   * Abstract PACT element representation
   * 
   * @param syncset //TODO  
   */
  abstract class PACT_Element(val syncset: SynchSets)



  /**
   * Process Algebra Tuple
   * 
   * @param p
   * @param syncset
   * @param definitions  
   */
  case class PAT(
    p: Process,
    override val syncset: SynchSets,
    definitions: List[Definition]
  ) extends PACT_Element(syncset)

  /**
   * Process Algebra Composition
   * 
   * @param lhs
   * @param syncset
   * @param hidden 
   * @param synched
   * @param rhs
   */
  case class PAC(
    lhs: PACT_Element,
    override val syncset: SynchSets,
    hidden: Set[Action],
    synched: Set[Action],
    rhs: PACT_Element) extends PACT_Element(syncset)

  /**
   * TODO: comment PACW 
   */
  case class PACW(
    wrappedProcessDefinition: String,
    pact: PACT_Element,
    override val syncset: SynchSets/*,
    elements: Iterable[PACT_Element]*/
  ) extends PACT_Element(syncset)

  /**
   * PACT Composer Trait
   */
  trait Composer {

    /**
     * defining the || infix Operator using implicits
     */ 
    implicit def cmp(pact: PACT_Element) = new Composition(pact)

    /**
     * Wrapper class to allow to provide a composition operator implicitly to a PACT element 
     */
    class Composition(lhs: PACT_Element) extends Composer {
      def ||(rhs: PACT_Element) = compose(lhs, rhs)
    }

    /** 
     * composes n PACT_Elements wrapped by an extra ProcessDefinition if wrappedProcessDefinition 
     * label is defined or else just returns the composed process
     */
    def composeN(wrappedProcessDefinition: Option[String], elements: Iterable[PACT_Element]): PACT_Element = {
      val pact = if (elements.size > 0) elements.reduceLeft((a, b) => compose(a, b)) else empty
      //val pact = elements.tail.foldLeft(elements.firstOption.getOrElse(empty))((a, b) => compose(a, b))
      if (!wrappedProcessDefinition.isDefined) pact
      else PACW(wrappedProcessDefinition.get, pact, pact.syncset/*, elements*/)
    }

    /**
     * defining the composition function used by the infix operator to generate PAC elements
     */  
    def compose(lhs: PACT_Element, rhs: PACT_Element): PACT_Element = {
      val consumed =
        lhs.syncset.consuming.map(a => a.label) & (rhs.syncset.synchaction.map(a => a.label) ++ rhs.syncset.unsynchaction.map(a => a.label)) ++
          rhs.syncset.consuming.map(a => a.label) & (lhs.syncset.synchaction.map(a => a.label) ++ lhs.syncset.unsynchaction.map(a => a.label))

      val consumed_actions =
        lhs.syncset.consuming.filter(e => consumed.contains(e.label)).toList :::
          lhs.syncset.synchaction.filter(e => consumed.contains(e.label)).toList :::
          lhs.syncset.unsynchaction.filter(e => consumed.contains(e.label)).toList :::
          rhs.syncset.consuming.filter(e => consumed.contains(e.label)).toList :::
          rhs.syncset.synchaction.filter(e => consumed.contains(e.label)).toList :::
          rhs.syncset.unsynchaction.filter(e => consumed.contains(e.label)).toList

      val synched =
        lhs.syncset.synchaction.map(a => a.label) & rhs.syncset.synchaction.map(a => a.label) ++ consumed_actions.map(a => a.label)

      val synched_actions =
        (lhs.syncset.synchaction ++ rhs.syncset.synchaction ++ consumed_actions).filter(a => synched.contains(a.label))

      val consuming_action = (lhs.syncset.consuming ++ rhs.syncset.consuming) -- consumed_actions
      val syncset_action = (lhs.syncset.synchaction ++ rhs.syncset.synchaction) -- consumed_actions
      val unsyncset_action = (lhs.syncset.unsynchaction ++ rhs.syncset.unsynchaction) -- consumed_actions

      
      val immediateactions = (lhs.syncset.consuming ++ lhs.syncset.synchaction ++ lhs.syncset.unsynchaction ++
      rhs.syncset.consuming ++ rhs.syncset.synchaction ++ rhs.syncset.unsynchaction) collect {
        case i : IA => i.label 
      }

      val hidden = immediateactions & consumed
      val hidden_actions = Set(consumed_actions.filter(p => hidden.contains(p.label)): _*)

      val localsynchset = SynchSets(consuming_action, syncset_action, unsyncset_action)
      PAC(lhs, localsynchset, hidden_actions, synched_actions, rhs)
    }
  }

  /**
   * PACT ReFiner: determines synchronizations more finegrained... 
   */
  trait ReFiner {
    def refine(pact: PACT_Element): PACT_Element = refine(pact, Set())

    def refine(pact: PACT_Element, backpropagation: Set[Action]): PACT_Element = pact match {
      case PAC(lhs, localsynchset, hidden_actions, synched_actions, rhs) => {
        val newbpg = backpropagation ++ synched_actions
        val additionalhides = synched_actions.map(_.label) -- backpropagation.map(_.label)
        val newhides = synched_actions.filter(a => additionalhides.contains(a.label))

        val pac = PAC(
          refine(lhs, newbpg),
          localsynchset, newhides, synched_actions,
          refine(rhs, newbpg))
        pac
      }
      case a => a
    }
  }

  /**
   * PACT decomposer trait: decomposing a PACT structure by process algebra term replacement
   */
  trait DeComposer {
    /**
     * Intermediate decomposition step for PACW structures
     * 
     * (used by the decomposition methods)
     * 
     * @param pacw 
     * @return [[PAT]]
     */
    private def decomposePACW(pacw: PACW) = {
      val pat = decompose(pacw.pact)
      val mPD = {
        (pacw.wrappedProcessDefinition, List[PPD]()) := List(pat.p)
      }

      PAT(I(pacw.wrappedProcessDefinition), pacw.syncset, pat.definitions ::: List(mPD))
    }

    /**
     * Recursive Decomposition of PACT structures including Hiding
     * 
     * @param pact A PACT structure
     * @return [[PAT]]
     */
    def decompose(pact: PACT_Element): PAT = pact match {
      case p: PAT => p
      case PAC(lhs, synchset, hidden, synched, rhs) => {
        val a = decompose(lhs)
        val b = decompose(rhs)
        
        (a,b) match {
          case (a,b) if a.p == Stop & b.p == Stop & synched.isEmpty => empty
          case (a,b) if b.p == Stop & synched.isEmpty => a
          case (a,b) if a.p == Stop & synched.isEmpty => b
          case _ => {
            if (hidden.size > 0)
	          PAT(Hiding(hidden.map(_.label), a.p | synched.map(_.label) | b.p), synchset, a.definitions ++ b.definitions)
	        else
	          PAT(a.p | synched.map(_.label) | b.p, synchset, a.definitions ++ b.definitions)
          } 
        }
      }
      case pacw: PACW => decomposePACW(pacw)
    }

    
    /**
     * Recursive Decomposition of PACT structures without Hiding
     * 
     * @param pact A PACT structure
     * @return [[PAT]]
     */
    def decomposeNoHide(pact: PACT_Element): PAT = pact match {
      case p: PAT => p
      case PAC(lhs, synchset, hidden, synched, rhs) => {
        val a = decomposeNoHide(lhs)
        val b = decomposeNoHide(rhs)
        
        (a,b) match {
          case (a,b) if a.p == Stop & b.p == Stop & synched.isEmpty => empty
          case (a,b) if b.p == Stop & synched.isEmpty => a
          case (a,b) if a.p == Stop & synched.isEmpty => b
          case _ => PAT(a.p | synched.map(_.label) | b.p, synchset, a.definitions ++ b.definitions)
        }
      }
      case pacw: PACW => decomposePACW(pacw)
    }
  }

  
  /**
   * PACT Serializer 
   */
  class PactSerialize(pact: PACT_Element) {
    
    /** 
     * Convert a PACT element to DOT format using a default name "default"
     * 
     * @return The DOT representation of the PACT structure 
     */
    def toDot(): String = toDot("default")
    
    /** 
     * Convert a PACT element to DOT format using a given name 
     *
     * @param name A specific name for the DOT output
     * @return The DOT representation of the PACT structure 
     */
    def toDot(name : String): String = {
      "digraph G {\n" + 
        toDotRek(name,pact) +
        "\n}"
    }
    
    /**
     * Convert a PACT structure recursively to DOT format using a given name
     * 
     * @param id The name
     * @param pact A pact structure 
     * @return The DOT representation of the PACT structure
     */
    private def toDotRek(id : String,pact: PACT_Element): String = {

      pact match {
        case PAT(p, d, e) => {
          val node = p match {
            case instantiation: I => id + "[shape=box, label=\"" + instantiation.variable + " " + "\"]"
            case Stop => id + "[style=filled shape=hexagon color=red fontcolor=red fillcolor=black label=\"" + "stop" + "\"]"
          }

          node
        }

        case PACW(l, pact, s/*, elements*/) => {
          val node = id + " [shape=box, label = \"" + l + " " + "\"]"

          /*
          val zippedPacts = elements.zipWithIndex          
          
          val dotPACTS = (for (p <- zippedPacts) yield toDotRek(id + p._2,p._1))

          val edges = (for (p <- zippedPacts) yield {
            id + p + "->" + id
          }).toList
           */

          val dot = node /*:: edges*/

          dot.mkString("  ", "\n  ", "")
        }

        case PAC(lhs, synchsets, hidden, synched, rhs) => {
          val dotRHS = toDotRek(id + "r",rhs)
          val dotLHS = toDotRek(id + "l",lhs)

          val edgeRHS = id + "r" + "->" + id +"s"
          val edgeLHS = id + "l" + "->" + id +"s"
          
          val synchnode = id+"s" + "  [style=filled shape=box fillcolor=grey label=\"|[" + synched.map(_.label).mkString("", ",", "") + "]|\"]"
          val hidenode = id + "  [style=filled shape=box fillcolor=grey label=\"hide " + hidden.map(_.label).mkString("", ",", "") + "\"]"
          
          val edgeSH = id+"s" + "->" + id 
          val subgraph = 
            "subgraph cluster_" + id + " {style=filled;color=lightgrey;\n" + 
            		edgeSH + ";\n" +
            		"struct" + id +  "[ label=\"{" + synchsets.toString + "}\""+ "shape=record;];\n" +
        	"}"
          
          
          val dot = List(edgeRHS, edgeLHS, subgraph, synchnode, hidenode, dotRHS, dotLHS)

          dot.mkString("  ", "\n  ", "")
        }
      }
    }
  }
}

