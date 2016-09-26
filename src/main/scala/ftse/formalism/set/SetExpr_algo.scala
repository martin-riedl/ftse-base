package ftse.formalism.set

import ftse.formalism.arith._
import ArithExpr_metamodel._

import SetExpr_metamodel._

/**
 * Provides methods implicitly to operate on a set expression
 */
trait SetExpr_algo {
  implicit def algorithms(se: SetExpr) = new SetAlgo(se)
}

/**
 * Provides methods to operate on a set expression
 * @param se A set expression  
 */
class SetAlgo(se: SetExpr) extends SE_algo {
  /**
   * Evaluates a set expression 
   * 
   * @param modify A function that modifies an arithmetic expression, implicitly the identity function is default 
   * @return Either a set of Integers or a set of Doubles
   */
  def eval(modify: ArithExpr => ArithExpr = identity[ArithExpr] _) = evalRek(se, modify)
  
  /**
   * Substitutes arithmetic expressions in the leaf elements
   * @param m A function that modifies an arithmetic expression 
   * @return [[SetExpr]]  
   */
  def subst(m: ArithExpr => ArithExpr) : SetExpr = substRek(se,m)
  
  /**
   * Checks whether a set expression can be evaluated 
   * @param isSetExprEvaluable A function to check whether the leaf element of the set expression can be evaluated, the implicit default maps to false
   * @param isArithExprEvaluable A function to check whether the leaf element contains arithmetic expressions that can be evaluated, the implicit default maps to false
   */
  def isEvaluable(
      isSetExprEvaluable : SetExpr => Boolean = (_) => false,
      isArithExprEvaluable : ArithExpr => Boolean = (_) => false
  ) = isEvaluableRek(se,isSetExprEvaluable, isArithExprEvaluable)
  
  /**
   * extracts the dependencies of the arithmetic expressions inside the leaf elements of the set expression
   * 
   * @param n A function that extracts a set of variable identifiers from an arithmetic expression, the implicit default maps to the empty set
   * @return set of dependencies in terms of strings
   */
  def dependsOn(n : ArithExpr => Set[String] = (_) => Set()) = dependsOnRek(se,n)
}

/**
 * Set Expression Algorithms 
 */
trait SE_algo extends ArithExpr_algo {
  private def calcSet[T](a: Either[Set[Int], Set[Double]], b: Set[T], op: Set[T] => Set[T]): Either[Set[Int], Set[Double]] = {
    /*
    if (!b.isEmpty) {
      val sth = op(b)
      
      (sth, sth.head) match { // trick 17 to elude type erasure
        case (r: Set[Int], foo: Int) => Left(r)
        case (r: Set[Double], foo: Double) => Right(r)
      }
    } else a
    
    */
    val sth = op(b)
    if (!sth.isEmpty) {  
      (sth, sth.head) match { // trick 17 to elude type erasure
        case (r: Set[Int], foo: Int) => Left(r)
        case (r: Set[Double], foo: Double) => Right(r)
      }
    } else a    
  }
  
  protected def dependsOnRek(se: SetExpr, n : ArithExpr => Set[String]) : Set[String] = se match {
    case SetFilter(a, _, b) => dependsOnRek(a,n)
    case SetDiff(a, b) => dependsOnRek(a,n) ++ dependsOnRek(b,n)
    case SetUnion(a, b) => dependsOnRek(a,n) ++ dependsOnRek(b,n)
    case SetIntersect(a, b) => dependsOnRek(a,n) ++ dependsOnRek(b,n)
    case SetRange(a, b) => a.dependsOn(n) ++ b.dependsOn(n)
    case SetBase(lst) => lst.map(_.dependsOn(n)).reduceLeft((a,b) => a ++ b)
  }
  
  protected def isEvaluableRek(se: SetExpr, m : SetExpr => Boolean, n : ArithExpr => Boolean) : Boolean = se match {
    case SetFilter(a, _, b) => isEvaluableRek(a,m,n) && b.isEvaluable(n)
    case SetDiff(a, b) => isEvaluableRek(a,m,n) && isEvaluableRek(b,m,n)
    case SetUnion(a, b) => isEvaluableRek(a,m,n) && isEvaluableRek(b,m,n)
    case SetIntersect(a, b) => isEvaluableRek(a,m,n) && isEvaluableRek(b,m,n)
    case SetRange(a, b) => a.isEvaluable(n) && b.isEvaluable(n)
    case SetBase(lst) => lst.map(_.isEvaluable(n)).reduceLeft((a,b) => a && b)
  }
  
  protected def evalRek(se: SetExpr, modify: ArithExpr => ArithExpr): Either[Set[Int], Set[Double]] = se match {
    case SetFilter(se, cmp, a) =>{ 
        (evalRek(se, modify),a.eval(modify)) match {
      	  case (Left(x),Left(y)) => {
      	    if (cmp == Comparator.>=) Left(x.filter(_ >= y))
      	    if (cmp == Comparator.<=) Left(x.filter(_ <= y))
      	    if (cmp == Comparator.!=) Left(x.filter(_ != y))
      	    else Left(x.filter(_ == y))
      	  }
      	  
      	  case (Right(x),Right(y)) => {
      	    if (cmp == Comparator.>=) Right(x.filter(_ >= y))
      	    if (cmp == Comparator.<=) Right(x.filter(_ <= y))
      	    if (cmp == Comparator.!=) Right(x.filter(_ != y))
      	    else Right(x.filter(_ == y))
      	  }      	
      	}
      }
      
    case SetDiff(a, b) =>
      val ea = evalRek(a, modify); (ea, evalRek(b, modify)) match {
        case (Left(x), Left(y)) => calcSet[Int](ea, y, (x.--))
        case (Right(x), Right(y)) => calcSet[Double](ea, y, (x.--))
        case _ => ea
      }

    case SetUnion(a, b) =>
      val ea = evalRek(a, modify); (ea, evalRek(b, modify)) match {
        case (Left(x), Left(y)) => calcSet[Int](ea, y, (x.++))
        case (Right(x), Right(y)) => calcSet[Double](ea, y, (x.++))
        case _ => ea
      }

    case SetIntersect(a, b) =>
      val ea = evalRek(a, modify); (ea, evalRek(b, modify)) match {
        case (Left(x), Left(y)) => calcSet[Int](ea, y, (x.&))
        case (Right(x), Right(y)) => calcSet[Double](ea, y, (x.&))
        case _ => ea
      }

    case SetRange(a, b) =>
      val ea = a.eval(modify); (ea, b.eval(modify)) match {
        case (Left(x), Left(y)) => Left(Set(x to y: _*))
        case _ => Left(Set())
      }

    case SetBase(lst) => {
      val ints = lst.map(a => a.eval(modify)).collect {case Left(i) => i}
      val dbls = lst.map(a => a.eval(modify)).collect {case Right(i) => i}

      if (!dbls.isEmpty) Right(Set(dbls: _*))
      else Left(Set(ints: _*))
    }
  }
  
  protected def substRek(se: SetExpr, m: ArithExpr => ArithExpr): SetExpr = se match {
    case SetFilter(a,cmp, b) => SetFilter(substRek(a,m), cmp, b.subst(m))
    case SetDiff(a, b) => SetDiff(substRek(a,m), substRek(b,m))
    case SetUnion(a, b) => SetUnion(substRek(a,m), substRek(b,m))
    case SetIntersect(a, b) => SetIntersect(substRek(a,m), substRek(b,m))
    case SetRange(a, b) => SetRange(a.subst(m), b.subst(m))
    case SetBase(lst) =>     SetBase(lst.map(_.subst(m)))
  }
}