package ftse.formalism.logical

import LE_metamodel._

trait LogicExpr_algo {
  implicit def algorithms(lexp: LogicalExpr) = new LExprAlgo(lexp)
}

class LExprAlgo(lexp: LogicalExpr) extends LogicExprAlgo {
  def modify(matcher: LogicalExpr => LogicalExpr = identity[LogicalExpr](_)) = modifyRek(lexp, matcher)
  def isEvaluable(isEvaluable : LogicalExpr => Boolean = (_) => false) = isEvaluableRek(lexp, isEvaluable)
  def containsAtom(checkAtom : LogicalExpr => Boolean) = containsAtomRek(lexp, checkAtom)
}

trait LogicExprEval {
  def evalAtom(atom: LExpAtom): Boolean

  def eval(lexp: LogicalExpr) = evalRek(lexp)
  def evalRek(lexp: LogicalExpr): Boolean = lexp match {
    case Conjunct(lhs, rhs) => evalRek(lhs) && evalRek(rhs)
    case Disjunct(lhs, rhs) => evalRek(lhs) || evalRek(rhs)
    case Negate(entry) => !evalRek(entry)
    case atom: LExpAtom => evalAtom(atom)
  }
}

object LExpReduce {
  //def reduce(lexp: LogicalExpr) = reduceRekBdd(reduceRek(lexp)) //BUG!
  def reduce(lexp: LogicalExpr) = reduceRek(reduceRek(lexp))
  
  def reduceRek(lexp: LogicalExpr): LogicalExpr = lexp match {
    case Conjunct(lhs,NeutralAtom) => reduceRek(lhs) 
    case Conjunct(_,BooleanAtom(false)) => BooleanAtom(false)
    case Conjunct(lhs,BooleanAtom(true)) => reduceRek(lhs)
    
    case Conjunct(BooleanAtom(false),_) => BooleanAtom(false)
    case Conjunct(BooleanAtom(true),rhs) => reduceRek(rhs)
    case Conjunct(NeutralAtom,rhs) => reduceRek(rhs) 
    
    case Conjunct(lhs,rhs) => Conjunct(reduceRek(lhs),reduceRek(rhs))

    case Disjunct(lhs,BooleanAtom(false)) => reduceRek(lhs)
    case Disjunct(_,BooleanAtom(true)) => BooleanAtom(true)
    case Disjunct(lhs, NeutralAtom) => reduceRek(lhs)
    
    case Disjunct(BooleanAtom(false),rhs) => reduceRek(rhs)
    case Disjunct(BooleanAtom(true),_) => BooleanAtom(true)
    case Disjunct(NeutralAtom, rhs) => reduceRek(rhs)
    
    case Disjunct(lhs,rhs) => Disjunct(reduceRek(lhs),reduceRek(rhs))

    case Negate(BooleanAtom(false)) => BooleanAtom(true)
    case Negate(BooleanAtom(true)) => BooleanAtom(false)
    case Negate(NeutralAtom) => NeutralAtom
    
    case Negate(entry) => Negate(reduceRek(entry))
    
    case atom => atom 
  }
  
  def reduceRekBdd(lexp: LogicalExpr): LogicalExpr = {
    val lexpbdd = new LogicExprBdd(lexp)
    lexpbdd.toLogicExpr()
  }
}

trait Gather[T]{
  protected def gatherAtom(sth : LExpAtom ) : Set[T]
  
  def gather(lexp: LogicalExpr) : Set[T] = gatherRek(lexp)
  
  protected def gatherRek(lexp: LogicalExpr): Set[T] = lexp match {
    case Conjunct(lhs, rhs) => gatherRek(lhs) ++ gatherRek(rhs)
    case Disjunct(lhs, rhs) => gatherRek(lhs) ++ gatherRek(rhs)
    case Negate(sth) => gatherRek(sth)
    case atom : LExpAtom => gatherAtom(atom)
  }
}
  
trait LogicExprAlgo {
  protected def modifyRek(l: LogicalExpr, f: LogicalExpr => LogicalExpr): LogicalExpr = l match {
    case Conjunct(lhs, rhs) => f(Conjunct((modifyRek(lhs, f)), (modifyRek(rhs, f))))
    case Disjunct(lhs, rhs) => f(Disjunct((modifyRek(lhs, f)), (modifyRek(rhs, f))))
    case Negate(expr) => f(Negate((modifyRek(expr, f))))
    case expr => f(expr)
  }
  
  protected def isEvaluableRek(l: LogicalExpr, f: LogicalExpr => Boolean): Boolean = l match {
    case Conjunct(lhs, rhs) => isEvaluableRek(lhs,f) && isEvaluableRek(rhs,f)
    case Disjunct(lhs, rhs) => isEvaluableRek(lhs,f) && isEvaluableRek(rhs,f)
    case Negate(expr) => isEvaluableRek(expr,f)
    case expr => f(expr)
  }
  
  protected def containsAtomRek(l : LogicalExpr, f : LogicalExpr => Boolean) : Boolean = l match {
    case Conjunct(lhs, rhs) => containsAtomRek(lhs,f) | containsAtomRek(rhs,f)
    case Disjunct(lhs, rhs) => containsAtomRek(lhs,f) | containsAtomRek(rhs,f)
    case Negate(expr) => containsAtomRek(expr,f)
    case expr => f(expr)
  }
}

abstract class AbstrLogicExprBdd(le: LogicalExpr) {
  import jdd.bdd._

  val bdd: BDD = new BDD(10000)

  // Hashmaps to save the relationship between Atom and the Integer representing the variable
  val var_hash = new scala.collection.mutable.HashMap[Int, LExpAtom]
  val name_hash = new scala.collection.mutable.HashMap[LExpAtom, Int]
  val root = rek_toBDD(le)
  
  println(le.toString)
  println("Root: " +root)

  private def rek_toBDD(le: LogicalExpr): Int = le match {
    //case for logical or
    case Disjunct(l: LogicalExpr, r: LogicalExpr) => {
      bdd or (rek_toBDD(l), rek_toBDD(r))
    }
    //case for logical and
    case Conjunct(l: LogicalExpr, r: LogicalExpr) => {
      bdd and (rek_toBDD(l), rek_toBDD(r))
    }
    //case for logical not
    case Negate(arg: LogicalExpr) => {
      bdd not (rek_toBDD(arg))
    }
    
    case atom: LExpAtom => rek_toBDD(atom)
  }

  private def rek_toBDD(elem: LExpAtom): Int = elem match {
    case BooleanAtom(v) => if (v==true) 1 else 0  // MARTIN: August 2013
    case le: LExpAtom => {
      val v = name_hash.get(le)
      if (v.isDefined) {
        //if the given String already exists return that variable
        return v.get
      } else {
        //else create a new variable and save the relationship in a HashMap
        val v = bdd.createVar()
        var_hash += ((v, le))
        name_hash += ((le, v))
        v
      }
    }
  }

  def reachReverseBDD(withFinalNodes : Boolean = false) = {
    // reachable graph  
    
    val G = new scala.collection.mutable.HashMap[Int, List[(Int, Boolean)]]()
    //G.put(root, List[(Int, Boolean)]())
    
    if (root==1) {
      val x = 1
    }
    if (root!=1 && root!=0) { // root node should not be a leaf node MARTIN August 2013
    
		//HashSet containing all nodes of the given BDD
		val marked = new scala.collection.mutable.HashSet[Int]()
		//a queue with the getHighs and getLows of the actual node
		val queue = new scala.collection.mutable.Queue[Int]()
		//actual node is added to the queue and the actual node is added to the marked
		
		queue.enqueue(root); marked += root
		
		
		//the breadthsearch to build up the marked list
		do {
		  val node = queue.dequeue
		  val high_node = bdd.getHigh(node)
		  val low_node = bdd.getLow(node)
		  
		  // update reachability graph
		  G.put(high_node, ((node, true) :: G.get(high_node).getOrElse(List[(Int, Boolean)]())))
		  G.put(low_node, ((node, false) :: G.get(low_node).getOrElse(List[(Int, Boolean)]())))
		
		  // enqueue the highnode if not already processed
		  if (high_node > 1 && !marked.contains(high_node)) { queue.enqueue(high_node); marked += high_node }
		
		  // enqueue the lownode if not already processed
		  if (low_node > 1 && !marked.contains(low_node)) { queue.enqueue(low_node); marked += low_node }
		
		} while (!queue.isEmpty)
		
		if (withFinalNodes) {
		  val finalNodes = marked -- G.keySet
		  finalNodes.foreach(G.put(_, List[(Int, Boolean)]())) // fill with empty list
		}     
  
    }
    
    scala.collection.immutable.HashMap[Int, List[(Int, Boolean)]](G.toList: _*)
    
  }
  
  /*
  def reachReverseBDD_inkl() = {
    //HashSet containing all nodes of the given BDD
    val marked = new scala.collection.mutable.HashSet[Int]()
    //a queue with the getHighs and getLows of the actual node
    val queue = new scala.collection.mutable.Queue[Int]()
    //actual node is added to the queue and the actual node is added to the marked
    queue.enqueue(root); marked += root
    // REACHABILITY GRAPH 

    val G = new scala.collection.mutable.HashMap[Int, List[(Int, Boolean)]]()

    //the breadthsearch to build up the marked list
    do {
      val node = queue.dequeue
      val high_node = bdd.getHigh(node)
      val low_node = bdd.getLow(node)
      
      // update reachability graph
      G.put(high_node, ((node, true) :: G.get(high_node).getOrElse(List[(Int, Boolean)]())))
      G.put(low_node, ((node, false) :: G.get(low_node).getOrElse(List[(Int, Boolean)]())))

      // enqueue the highnode if not already processed
      if (!marked.contains(high_node)) { queue.enqueue(high_node); marked += high_node }
    
      // enqueue the lownode if not already processed
      if (!marked.contains(low_node)) { queue.enqueue(low_node); marked += low_node }

    } while (!queue.isEmpty)

    scala.collection.immutable.HashMap[Int, List[(Int, Boolean)]](G.toList: _*)
  }
  */
  
  def toLogicExpr() : LogicalExpr = {
    //HERE IS A THE BUG!!!!!
    
    val structure = reachReverseBDD(true)
    val var_hash_keys = var_hash.keys.toList.sortWith((e1, e2) => e1 < e2)
    
    // reach determines all nodes reachable by starting in a bdd terminal (i.e. default: TRUE terminal)
    // and recalculates their boolean expression structure 
    def breathreach(current: Int = 1) = {
      val marked = new scala.collection.mutable.HashSet[Int]()
      val queue = new scala.collection.mutable.Queue[Int]()
      queue.enqueue(current); marked += current
      
      val calculated = scala.collection.mutable.HashMap[Int, LogicalExpr]()
      var last : Option[Int] = None
      
      do {
        val child = queue.dequeue
        if (structure.contains(child)) {
          val parents = structure(child)
          for ((p,t) <- parents  if !marked.contains(p)) {
        	  queue.enqueue(p); marked += p
        	  
        	  
        	  // here comes the code to perform the calculation of the boolean expression structure for a variable node
        	  val lit = var_hash(var_hash_keys(bdd.getVar(p)))
        	  val dep = List(
	    			  calculated.get(bdd.getHigh(p)), 
	    			  calculated.get(bdd.getLow(p)).map(Negate(_))
	    	  ).filter(_.isDefined).map(_.get)
	    	  
	    	  val expr = if (dep.nonEmpty) Conjunct(lit , dep.reduce(Disjunct(_,_))) else lit
	    	  calculated.put(p, expr)  
	    	  last = Some(p)
	    	  // --------------------------
	    	  
	      }
        }        
      } while (!queue.isEmpty)
      
      last.map(calculated(_)).getOrElse(BooleanAtom(false))
    }
    
    breathreach()
    
    /*
    // sollte eigentlich topologisch sortiert erfolgen 
    // und eben nicht rekursiv gemacht ... momentan noch mehrfachberechnung möglich ... 
    def convert(current : Int) : Option[LogicalExpr] = {
      if (current<2) return None
      
      val high = bdd.getHigh(root)
      val low = bdd.getLow(root)
      
      val res1 = if (topologicalordered.contains(high)) convert(high) else None
      val res2 = if (topologicalordered.contains(low)) convert(low).map(Negate(_)) else None
      val successors = List(res1, res2).filter(_.isDefined).map(_.get)
      val root_exp = var_hash(var_hash_keys(bdd.getVar(current)))
      val result = if (successors.nonEmpty) Conjunct(root_exp,successors.reduceLeft((a,b) => Disjunct(a,b))) else root_exp 
       
      Some(result)
    }
    
    convert(root).get
    */
  }
  
  
  def satTerms(toTrue: Boolean = true): (List[LExpAtom], List[List[(LExpAtom, Boolean)]]) = {
    // obtain the reachability structure by calling reachReverseBDD
    val structure = reachReverseBDD()
    val structure_string = structure.map(a => a._1 + "->" + a._2).mkString("\n")
    println(structure)
    
    
    // sort variable hash keys
    val var_hash_keys = var_hash.keys.toList.sortWith((e1, e2) => e1 < e2)

    // rekursive function definition to determine sat paths
    def determinePaths_rek(current: Int = 1, ns: List[(Int, Boolean)] = List()): List[List[(Int, Boolean)]] = {
      val c = structure.get(current)
      if (c.isDefined) {
        c.get.flatMap(a => {
          determinePaths_rek(a._1, ns.+:(a))
        })
      } else List(ns)
    }

    // determine all paths by starting with either the 1 or the 0 leaf node
    val paths = determinePaths_rek(if (toTrue) 1 else 0)

    val named_paths = paths.map(p => p.map(entry => (var_hash(var_hash_keys(bdd.getVar(entry._1))), entry._2)))

    val variables = (0 to bdd.numberOfVariables() - 1).map(var_hash_keys(_)).map(var_hash(_)).toList

    (variables, named_paths)
  }

  def satMinTerms(toTrue: Boolean = true): (List[LExpAtom], List[List[(LExpAtom, Boolean)]]) = {
    // obtain the reachability structure by calling reachReverseBDD
    val structure = reachReverseBDD()
    // sort variable hash keys
    val var_hash_keys = var_hash.keys.toList.sortWith((e1, e2) => e1 < e2)
    def map(n: Int) = var_hash(var_hash_keys(bdd.getVar(n)))

    // rekursive function definition to determine sat paths
    def determinePaths_rek(current: Int = 1, ns: List[List[(Int, Boolean)]] = List(List())): List[List[(Int, Boolean)]] = {
      val c = structure.get(current)
      if (c.isDefined) {
        c.get.flatMap(a => {

          // determine ignored variable levels
          val currentlevel = bdd.getVar(current)
          val nextlevel = bdd.getVar(a._1)

          val difference = math.abs(currentlevel - nextlevel) - 1

          val dontCareLevels = (nextlevel + 1 to currentlevel - 1).toList

          def genPaths(l: List[Int]): List[List[(Int, Boolean)]] = {
            if (l.size == 0) List(List())
            else {
              val paths = genPaths(l.tail)
              val tp = paths.map(_.+:(l.head, true))
              val fp = paths.map(_.+:(l.head, false))
              tp ++ fp
            }
          }

          val dontCarePaths = genPaths(dontCareLevels)

          val paths = ns flatMap (s => {
            dontCarePaths.map(dcp => (dcp ++ s).+:((nextlevel, a._2)))
          }) 

          determinePaths_rek(a._1, paths)
        })
      } else ns
    }

    // determine all paths by starting with either the 1 or the 0 leaf node
    val paths = determinePaths_rek(if (toTrue) 1 else 0)

    val named_paths = paths.map(p => p.map(entry => (var_hash(var_hash_keys(entry._1)), entry._2)))

    val variables = (0 to bdd.numberOfVariables() - 1).map(var_hash_keys(_)).map(var_hash(_)).toList

    (variables, named_paths)
  }

  /* INCORPORATED LOGEXPATOMS
  def reachReverseBDD2() = {
	    // sort variable hash keys
	    val var_hash_keys = var_hash.keys.toList.sortWith((e1,e2)=>e1<e2)
	  	def map(i : Int) = var_hash(var_hash_keys(bdd.getVar(i)))

	  	//HashSet containing all nodes of the given BDD
        val marked = new scala.collection.mutable.HashSet[(LExpAtom,Int)]()
        //a queue with the getHighs and getLows of the actual node
        val queue = new scala.collection.mutable.Queue[(LExpAtom,Int)]()
        //actual node is added to the queue and the actual node is added to the marked
        
        
        queue.enqueue((map(root),root)); marked += ((map(root),root))
        // REACHABILITY GRAPH 
       
        val G = new scala.collection.mutable.HashMap[(LExpAtom,Int),List[((LExpAtom,Int),Boolean)]]()
        
        //the breadthsearch to build up the marked list
        do {
          val node = queue.dequeue
          val high_node = bdd.getHigh(node._2)
          val hn = (map(high_node),high_node)
          val low_node = bdd.getLow(node._2)
          val ln = (map(high_node),high_node)
          
          // update reachability graph
          G.put(hn,  ((node,true) :: G.get(hn).getOrElse(List[((LExpAtom,Int),Boolean)]())))
          G.put(ln,  ((node,false) :: G.get(ln).getOrElse(List[((LExpAtom,Int),Boolean)]())))
          
          //the gethigh of the node
          if (bdd.getHigh(node._2)>1 && !marked.contains((map(high_node),high_node))) {
            
            
            queue.enqueue(hn); marked += hn
          }
          //the getlow of the node
          if (bdd.getLow(node._2)>1 && !marked.contains(ln)) {
            
            queue.enqueue(ln); marked += ln
          }
          
        } while (!queue.isEmpty)
       
        scala.collection.immutable.HashMap[(LExpAtom,Int),List[((LExpAtom,Int),Boolean)]](G.toList :_*)
  }
  */

}

class LogicExprBdd(le: LogicalExpr) extends AbstrLogicExprBdd(le)

