package ftse.transformations

import ftse.tools._

import ftse.formalism.arith._
import ArithExpr_metamodel._

import ftse.formalism.logical._
import LE_metamodel._

import ftse.formalism.spa._
import scala.collection.immutable._
import jdd.bdd._

trait Convert extends SPA {
  import SPA_metamodel._
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.HashSet
  import scala.collection.mutable.Queue

  /**
    * @param le: logical state expression representing a condition
    * @param traverser: a function traversing the bdd from the root node generating guarded processes
    * @return a list with the generated guarded processes
    */

  type variable = Int

  def convert(le: LogicalExpr, name: String): Iterable[Iterable[(LExpAtom, Boolean)]] = {
    //Definition of a BDD with a maximal nodesize of 10000
    val bdd = new BDD(10000)
    // Hashmaps to save the relationship between Atom and the Integer representing the variable
    val var_hash = new HashMap[variable, LExpAtom]
    val name_hash = new HashMap[LExpAtom, variable]

    /*
		 * inner recursive function with sideeffects on var_hash and name_hash that stores a mapping
		 * node -> identifier, identifier -> node
		 * @param le: logical state expression
		 * @returns: root node
		 */
    val countstatespercomponent = HashMap[Iterable[String], Iterable[Int]]()

    def rek_toBDD(le: LogicalExpr): Int = le match {
      //case for a boolean expression
      case atom: LExpAtom => {
        val v = name_hash.get(atom)
        if (v.isDefined)
          //if the given String already exists return that variable
          return v.get
        else {
          //else create a new variable and save the relationship in a HashMap
          val v = bdd.createVar()

          atom match {
            case acis: AtomState => {
              countstatespercomponent(acis.ci) = List(v) ++ countstatespercomponent.getOrElse(acis.ci, List())
            }
            case _ =>
          }

          var_hash += ((v, atom))
          name_hash += ((atom, v))
          v
        }
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

    val root = rek_toBDD(le)

    val paths = GenPaths(root, bdd, var_hash, countstatespercomponent)
    val var_hash_keys = var_hash.keys.toList.sortWith((e1, e2) => e1 < e2)

    paths.map(a => a.toList.map(a => (a._2, a._3)))
  }

  def GenPaths(
    root: Int,
    bdd: BDD,
    var_hash: HashMap[variable, LExpAtom],
    countstatespercomponent: HashMap[Iterable[String], Iterable[Int]]) = {
    //HashSet containing all nodes of the given BDD
    val marked = new HashSet[Int]()
    //a queue with the getHighs and getLows of the actual node
    val queue = new Queue[Int]()
    //actual node is added to the queue and the actual node is added to the marked
    queue.enqueue(root); marked += root

    val symbolicTrue = bdd.getOne()
    val var_hash_keys = var_hash.keys.toList.sortWith((e1, e2) => e1 < e2)
    type LiteralType = (Int, LExpAtom, Boolean)
    type IterableLiterals = Iterable[LiteralType]
    val parents_mapping = new HashMap[Int, IterableLiterals]()
    //modified breadthsearch to build up the marked list
    do {
      val originalnode: Int = queue.dequeue
      type EntryType = (Int, IterableLiterals)
      var highentry: EntryType = null
      var lowentry: EntryType = null
      val nodevar = bdd.getVar(originalnode)

      val high_node: Int = bdd.getHigh(originalnode)
      val high_nodevar = bdd.getVar(high_node)

      // introduce intermediate splitstates for the high-part
      var node = originalnode
      var nodeatom = var_hash(var_hash_keys(bdd.getVar(node)))

      //println("CURRENT NODE",originalnode,bdd.getVar(originalnode),nodeatom)
      //println("HIGH_NODE",high_node,high_nodevar)

      val bypassed_variable_levels_high = bdd.getVar(originalnode) + 1 to bdd.getVar(high_node) - 1

      //println("BYPASSED_LEVELS: " + bypassed_variable_levels_high.size)

      for (successor <- bypassed_variable_levels_high) {
        highentry = (successor + 1000, (List((node, nodeatom, true)) ++ parents_mapping.getOrElse(successor + 1000, List())))
        parents_mapping += highentry
        node = successor + 1000
        nodeatom = var_hash(var_hash_keys(successor))

        if (bypassed_variable_levels_high.last < successor)
          lowentry = (successor + 1001, (List((node, nodeatom, false)) ++ parents_mapping.getOrElse(successor + 1001, List())))
        else
          lowentry = (high_node, (List((node, nodeatom, false)) ++ parents_mapping.getOrElse(high_node, List())))

        parents_mapping += lowentry
      }

      highentry = (high_node, (List((node, nodeatom, true)) ++ parents_mapping.getOrElse(high_node, List())))
      parents_mapping += highentry

      // introduce intermediate splitstates for the low-part

      node = originalnode
      nodeatom = var_hash(var_hash_keys(bdd.getVar(node)))
      val low_node: Int = bdd.getLow(originalnode)
      var low_nodevar = bdd.getVar(low_node)
      //          println("LOW_NODE",low_node,low_nodevar)

      val bypassed_variable_levels_low = bdd.getVar(originalnode) + 1 to bdd.getVar(low_node) - 1

      //          println("BYPASSED_LEVELS: " + bypassed_variable_levels_low.size)

      for (successor <- bypassed_variable_levels_low) {
        lowentry = (successor + 2000, (List((node, nodeatom, false)) ++ parents_mapping.getOrElse(successor + 2000, List())))
        parents_mapping += lowentry
        highentry = (successor + 2000, (List((node, nodeatom, true)) ++ parents_mapping.getOrElse(successor + 2000, List())))
        parents_mapping += highentry
        node = successor + 2000
        nodeatom = var_hash(var_hash_keys(successor))
      }

      lowentry = (low_node, (List((node, nodeatom, false)) ++ parents_mapping.getOrElse(low_node, List())))
      parents_mapping += lowentry

      //          println(parents_mapping)

      //the gethigh of the node 
      if (high_node > 1 && !marked.contains(high_node)) {
        queue.enqueue(high_node); marked += high_node
      }
      //the getlow of the node
      if (low_node > 1 && !marked.contains(low_node)) {
        queue.enqueue(low_node); marked += low_node
      }
    } while (!queue.isEmpty)

    val l = marked.toList

    val current = symbolicTrue

    // println(parents_mapping)

    def paths(c: Int, path: scala.collection.immutable.Stack[(Int, LExpAtom, Boolean)]): Iterable[scala.collection.immutable.Stack[(Int, LExpAtom, Boolean)]] = {
      //println(c)
      if (c == root) {
        //println("test")
        List(path)
      } else {
        val parents = parents_mapping(c)
        parents flatMap(p => paths(p._1, path).map(a => a.push(p)))
      }
    }
    val blub = paths(current, new scala.collection.immutable.Stack())
    blub
    //ps.map(a=>a.map(b=>var_hash(b._1)))
  }

  /*
       * <Description>
       * @param root: the Integer representation of the node with the depth 0
       * @param bdd: the created BDD
       * @param var_hash: Hashmap over [int, identifier] for getting the names to the nodes
       * @results a list of guarded processes
       */
  def breadthFirst(
    root: Int,
    bdd: BDD,
    var_hash: HashMap[Int, String],
    name: String): (List[GuardedProcess], scala.collection.immutable.Set[Action]) =
    {
      //HashSet containing all nodes of the given BDD
      val marked = new HashSet[Int]()
      //a queue with the getHighs and getLows of the actual node
      val queue = new Queue[Int]()
      //list with all keys of all variables of the given BDD
      val var_hash_keys = var_hash.keys.toList.sortWith((e1, e2) => e1 < e2)
      //actual node is added to the queue and the actual node is added to the marked
      queue.enqueue(root); marked += root
      //a Hashmap to link the spa state to the nodes
      val bddspanodemapping = new HashMap[Int, Int]()
      var i = 1
      //the breadthsearch to build up the marked list
      do {
        val node = queue.dequeue
        val x = (node -> i)
        bddspanodemapping += x

        //println("actual node:"+node+" getHigh:"+bdd.getHigh(node)+" getLow"+bdd.getLow(node))

        i += 1
        //the gethigh of the node
        if (bdd.getHigh(node) > 1 && !marked.contains(bdd.getHigh(node))) {
          val high_node = bdd.getHigh(node)
          queue.enqueue(high_node); marked += high_node
        }
        //the getlow of the node
        if (bdd.getLow(node) > 1 && !marked.contains(bdd.getLow(node))) {
          val low_node = bdd.getLow(node)
          queue.enqueue(low_node); marked += low_node
        }

      } while (!queue.isEmpty)

      //here is a sorted list with the order, which is needed in the spa
      val sorted_node_list = bddspanodemapping.keys.toList.sortWith((e1, e2) => (bddspanodemapping(e1) < bddspanodemapping(e2)))

      //in the bddspanodemapping is no GuardFalse and no GuardTrue, which is added here
      bddspanodemapping += (1 -> (marked.size + 1))
      bddspanodemapping += (0 -> 0)

      //a list of GuardedProcesses based on the sorted_node_list 
      val pl = for (node <- sorted_node_list) yield {
        val highnode = bdd.getHigh(node)
        val lownode = bdd.getLow(node)
        //the string of the actual node
        val label = var_hash(var_hash_keys(bdd.getVar(node))).mkString("", "_", "")

        //a GuardedProcess is created
        

        val condition = Condition(ArithAtomIdentifier("state"), ConditionOp.==, ArithAtomValue(Left(bddspanodemapping(node))))
        //val guardstr = "state=" + bddspanodemapping(node)
        
        (Guard(List(condition)),
          (IA(label, 1.0), I(name, if (bdd.getHigh(node) == 0) 0 else bddspanodemapping(highnode))),
          (IA(label + "_not", 1), I(name, if (bdd.getLow(node) == 0) 0 else bddspanodemapping(lownode))))
      }

      val gprocesses = pl.map(a => GuardedProcess(a._1, (a._2._1 / a._2._2) + (a._3._1 / a._3._2)))
      val synch = scala.collection.immutable.Set[Action]((pl.flatMap(a => List(a._2._1, a._3._1))): _*)

      //return value is the list of all GuardedProcesses
      (gprocesses, synch)
    }
}