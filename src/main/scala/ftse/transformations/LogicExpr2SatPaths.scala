package ftse.transformations

import ftse.formalism.arith._
import ftse.formalism.logical._
import LE_metamodel._

class SATPATH_Generator(elem: LogicalExpr) extends LogicExpr2SatPaths {
  def genSatPaths = getSatisfyablePaths(elem)
}

trait LogicExpr2SatPathsImpl {
  implicit def transform(e: LogicalExpr) = new SATPATH_Generator(e)
}

/**
 * Provides methods to convert logical expressions to satisfyable paths
 */
trait LogicExpr2SatPaths {
  import ftse.tools._
  import jdd.bdd._

  type ComponentLabel = Iterable[String]
  type StateLabel = String

  def filterAtom(lexp: LogicalExpr): Iterable[(Iterable[String], String)] = lexp match {
    case AtomState(component, state) => List((component -> state))
    case Negate(exp) => filterAtom(exp)
    case Conjunct(explhs, exprhs) => filterAtom(explhs) ++ filterAtom(exprhs)
    case Disjunct(explhs, exprhs) => filterAtom(explhs) ++ filterAtom(exprhs)
  }


  /**
	* Generates all paths from the LogicalExpression
	* 
	* @param lexp A logical expression 
	* @return 
	*/
  def getSatisfyablePaths(lexp: LogicalExpr): Iterable[scala.collection.immutable.Stack[(Int, (Iterable[String], String), Boolean)]] = {
    val bdd = new BDD(10000)
    val var_hash = scala.collection.mutable.HashMap[Int, (ComponentLabel, StateLabel)]()
    val name_hash = scala.collection.mutable.HashMap[(ComponentLabel, StateLabel), Int]()
    val component_state_mapping = scala.collection.mutable.HashMap[ComponentLabel, Iterable[StateLabel]]()

    // filters all atom elements from the logical state expression 
    // and build up an injective mapping between component labels and 
    // a list of state labels
    filterAtom(lexp).foreach { t =>
      val component = t._1; val state = t._2
      component_state_mapping(component) = List(state) ++ component_state_mapping.getOrElse(component, List())
    }

    // create all bdd variable ordered by the component labels
    // defining the encoding order for a logical expression
    component_state_mapping.keys.foreach { component =>
      val states = component_state_mapping(component)
      states.foreach { state =>
        {
          // create a mini-bdd consisting only of one node (i.e. the state)
          val bddvariable = bdd.createVar()
          //println(bddvariable,state)
          var_hash(bddvariable) = (component, state)
          name_hash((component, state)) = bddvariable
        }
      }
    }

    /**
	  * Encodes the LogicalExpression as BDD
	  * 
	  * @param lexp A logical expression 
	  * @return
	  */
    def rek_toBDD(lexp: LogicalExpr): Int = lexp match {
      case AtomState(component, state) => {
        //println("BLA" + name_hash)
        name_hash.get((component, state)).get
      }
      //case for logical or
      case Disjunct(l: LogicalExpr, r: LogicalExpr) => {
        val lBDD = rek_toBDD(l)
        val rBDD = rek_toBDD(r)

        val resBDD = bdd or (lBDD, rBDD)

        bdd.ref(resBDD)
        bdd.deref(lBDD)
        bdd.deref(rBDD)

        resBDD
      }
      //case for logical and
      case Conjunct(l: LogicalExpr, r: LogicalExpr) => {
        val lBDD = rek_toBDD(l)
        val rBDD = rek_toBDD(r)

        val resBDD = bdd and (lBDD, rBDD)

        bdd.ref(resBDD)
        bdd.deref(lBDD)
        bdd.deref(rBDD)

        resBDD
      }
      //case for logical not
      case Negate(arg: LogicalExpr) => {
        val argBDD = rek_toBDD(arg)
        val resBDD = bdd not (argBDD)
        bdd ref resBDD
        bdd deref argBDD
        resBDD
      }
      case _ => error("No LogicalStateExpression given.")
    }

    /**
	  * Generate all satisfying paths
	  * 
	  * @param root Root node of a bdd
	  * @param bdd The bdd
	  * @return //TODO
	  */
    def GenPaths(
      root: Int,
      bdd: BDD) = {
      BddLogger.clean
      BddLogger.log("  " + bdd.getOne() + "[rank=" + bdd.getVar(bdd.getOne()) + "]")
      BddLogger.log("  " + bdd.getZero() + "[rank=" + bdd.getVar(bdd.getZero()) + "]")

      type nodenum = Int

      //HashSet containing all nodes of the given BDD
      val marked = scala.collection.mutable.HashSet[nodenum]()
      //a queue with the getHighs and getLows of the actual node

      abstract trait TODO[A] {
        def insert(elem: A)
        def take: A
      }

      class Depth[A] extends scala.collection.mutable.Stack[A] with TODO[A] {
        def insert(elem: A) = push(elem)
        def take: A = pop()
        override def isEmpty = super.isEmpty
      }

      class Breadth[A] extends scala.collection.mutable.Queue[A] with TODO[A] {
        def insert(elem: A) = enqueue(elem)
        def take: A = dequeue()
        override def isEmpty = super.isEmpty
      }

      val todo = new Depth[nodenum]()
      //actual node is added to the queue and the actual node is added to the marked
      //todo.enqueue(root); //queue
      todo.insert(root) // stack
      marked += root

      val symbolicTrue = bdd.getOne()
      val var_hash_keys = var_hash.keys.toList.sortWith((e1, e2) => e1 < e2)
      val parents_mapping = scala.collection.mutable.HashMap[nodenum, Set[(nodenum, (ComponentLabel, StateLabel), Boolean)]]()

      val group = (for (x <- 0 to name_hash.keys.toList.length - 1) yield {
        val bezeichner = var_hash(var_hash_keys(x))
        bezeichner._1.mkString("", "_", "") + "_" + bezeichner._2 + "[rank=" + x + ",shape=box]"
      }).toList

      val groupstruct = (for (x <- 0 to name_hash.keys.toList.length - 2) yield {
        val bezeichner = var_hash(var_hash_keys(x))
        val bezeichner2 = var_hash(var_hash_keys(x + 1))
        bezeichner._1.mkString("", "_", "") + "_" + bezeichner._2 + "->" + bezeichner2._1.mkString("", "_", "") + "_" + bezeichner2._2
      }).toList

      BddLogger.log("  {")
      group.foreach(a => BddLogger.log("    " + a))
      groupstruct.foreach(a => BddLogger.log("    " + a))
      BddLogger.log("  }")

      //println("VARHASH: "+var_hash)

      /**
       * determines the level of a node
       */
      def obtainLevel(e: Int) = {
        if (e == bdd.getOne | e == bdd.getZero) var_hash_keys.size
        else bdd.getVar(e)
      }

      def obtainNextLevel(currentindex: Int) = {
        val cur_key = var_hash_keys(currentindex)
        val (cur_complabel, cur_statelabel) = var_hash(cur_key)
        val nxt_index = currentindex + 1
        if (var_hash_keys.size > nxt_index) {
          //	    		val nxt_key = var_hash_keys(nxt_index)
          //	    		val (nxt_complabel, nxt_statelabel) = var_hash(nxt_key)
          //	    		if (nxt_complabel==cur_complabel) 
          Some(nxt_index)
          //		    	else None
        } else None
      }

      
      /**
       * //TODO
       */
      def obtainNextComponentLevel(currentindex: Int) = {
        val cur_key = var_hash_keys(currentindex)
        val (cur_complabel, cur_statelabel) = var_hash(cur_key)

        def obtainRek(index: Int): Option[Int] = {
          if (var_hash_keys.size > index) {
            val key = var_hash_keys(index)
            val (complabel, statelabel) = var_hash(key)
            if (complabel != cur_complabel) {
              Some(index)
            } else obtainRek(index + 1)
          } else None
        }

        obtainRek(currentindex + 1)
      }

      /**
       * //TODO
       */
      def introduce_intermediate(parent: Int, child: Int, high: Boolean) = {
        //println("INTRODUCE parent:"+parent+" parent_index:"+bdd.getVar(parent)+" child:"+child+" child_index:"+bdd.getVar(child))
        // maps level -> state
        val statemap = scala.collection.mutable.HashMap[Option[Int], Int]()
        // add the child node
        statemap(Some(bdd.getVar(child))) = child
        statemap(None) = child

        // Generate first Intermediate State
        var optionindex = if (high == true) obtainNextComponentLevel(obtainLevel(parent))
        else obtainNextLevel(obtainLevel(parent))

        //println(" nextindex"+optionindex)

        var intermediatestate = if (!optionindex.isDefined) child
        else if (bdd.getVar(child) - optionindex.get < 1) child
        else statemap.getOrElse(optionindex, StateFab.genNewState)

        //println("STATE:"+intermediatestate+" nextindex"+optionindex)

        if (high) BddLogger.log("  " + parent + "->" + intermediatestate + "[color=red]")
        else BddLogger.log("  " + parent + "->" + intermediatestate + "[color=red, style=dashed]")

        val entry = (intermediatestate, parents_mapping.getOrElse(intermediatestate, Set()) + ((parent, var_hash(var_hash_keys(bdd.getVar(parent))), high)))
        parents_mapping += entry

        val range = (if (optionindex.isDefined) optionindex.get else bdd.getVar(child)) to obtainLevel(child) - 1
        //println("RANGE "+range)

        for (index <- range) {
          // Generate following intermediate States 
          // generate next component state only once
          val ncv_index = obtainNextComponentLevel(index)
          val ncv_state = statemap.getOrElse(ncv_index, StateFab.genNewState)
          if (ncv_index.isDefined) BddLogger.log("  " + ncv_state + "[rank=" + ncv_index.get + "]")
          statemap(ncv_index) = ncv_state
          // generate next state only once
          val nv_index = obtainNextLevel(index)
          val nv_state = statemap.getOrElse(nv_index, StateFab.genNewState)
          if (nv_index.isDefined) BddLogger.log("  " + nv_state + "[rank=" + nv_index.get + "]")
          statemap(nv_index) = nv_state
          (ncv_state, nv_state)

          // generate bdd transition to next component state
          //println("NEXTCompSTATE "+ncv_state)
          BddLogger.log("  " + intermediatestate + "->" + ncv_state + "[color=red]")
          // generate bdd transition to next state
          //println("NEXTSTATE "+nv_state)
          BddLogger.log("  " + intermediatestate + "->" + nv_state + "[color=red, style=dashed]")

          val entryhigh = (ncv_state, parents_mapping.getOrElse(ncv_state, Set()) + ((intermediatestate, var_hash(var_hash_keys(index)), true)))
          parents_mapping += entryhigh

          val entrylow = (nv_state, parents_mapping.getOrElse(nv_state, Set()) + ((intermediatestate, var_hash(var_hash_keys(index)), false)))
          parents_mapping += entrylow

          //println(index,var_hash(var_hash_keys(index)))

          intermediatestate = nv_state
        }
      }

      /**
       * TODO
       */
      object StateFab {
        var statecounter: Int = 100000
        def genNewState = {
          statecounter += 1
          statecounter
        }
      }

      //modified breadthsearch to build up the marked list
      do {
        val originalnode: Int = todo.take
        var highentry: (nodenum, Set[(nodenum, (ComponentLabel, StateLabel), Boolean)]) = null
        var lowentry: (nodenum, Set[(nodenum, (ComponentLabel, StateLabel), Boolean)]) = null
        val nodevar = bdd.getVar(originalnode)

        val high_node: Int = bdd.getHigh(originalnode)
        val high_nodevar = bdd.getVar(high_node)

        introduce_intermediate(originalnode, high_node, true)

        val low_node: Int = bdd.getLow(originalnode)
        var low_nodevar = bdd.getVar(low_node)

        introduce_intermediate(originalnode, low_node, false)

        BddLogger.log("  " + originalnode + "[rank=" + bdd.getVar(originalnode) + "]")
        BddLogger.log("  " + originalnode + "->" + high_node)
        BddLogger.log("  " + originalnode + "->" + low_node + "[style=dashed]")

        //the gethigh of the node 
        if (high_node > 1 && !marked.contains(high_node)) todo.insert(high_node); marked += high_node
        //the getlow of the node
        if (low_node > 1 && !marked.contains(low_node)) todo.insert(low_node); marked += low_node

      } while (!todo.isEmpty)

      BddLogger.log("  " + bdd.getOne() + "[rank=" + bdd.getVar(bdd.getOne()) + "]")
      BddLogger.log("  " + bdd.getZero() + "[rank=" + bdd.getVar(bdd.getZero()) + "]")

      val l = marked.toList
      val current = symbolicTrue
      
      /**
	    * Recursive traversal off the resulting tree
	    * 
	    * @param c //TODO 
	    * @param path //TODO
	    * @return //TODO 
	    */
      def paths(c: Int, path: scala.collection.immutable.Stack[(nodenum, (ComponentLabel, StateLabel), Boolean)]): Iterable[scala.collection.immutable.Stack[(nodenum, (ComponentLabel, StateLabel), Boolean)]] = {
        if (c == root) {
          List(path)
        } else {
          val parents = parents_mapping(c)
          parents.flatMap(p => paths(p._1, path) map(a => a.push(p)))
        }
      }

      paths(current, new scala.collection.immutable.Stack())
    } // closes genpath

    // encode logical expression as bdd
    val root = rek_toBDD(lexp)
    // generate satisfiable paths from bdd
    val paths = GenPaths(root, bdd)
    //println(ftse.tools.BddLogger.getDot)
    paths

  } // closes genbdd
}