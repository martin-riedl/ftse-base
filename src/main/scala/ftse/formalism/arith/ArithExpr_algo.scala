package ftse.formalism.arith

import ArithExpr_metamodel._

/**
 * Defines an implicit conversion of arithmetic expressions such that a number 
 * of algorithms can be directly accessed provided by the instantiated wrapper class 
 */
trait ArithExpr_algo {
  implicit def algorithms(ae: ArithExpr) = new ArithAlgo(ae)
  implicit def a2[T](ae: AE[T]) = new AEAlgo[T](ae)
}

/**
 * wrapper class for the default arithmetic expression 
 */
class ArithAlgo(ae: ArithExpr) extends AE_algo {
  /**
   * An evaluation function for an arithmetic expression 
   * @param modify A function as an optional parameter which modifies an arithmetic expression
   */
  def eval(modify: ArithExpr => ArithExpr = identity[ArithExpr](_)) = evalRek(ae, modify)
  
  /**
   * A substitution function for an arithmetic expression 
   * @param modify A function as a parameter that modifies an arithmetic expression
   */
  def subst(modify : ArithExpr => ArithExpr) = substRek(ae,modify)
  
  
  /**
   * An function which determines whether an arithmetic expression is evaluable 
   * @param isEvaluable A function as a parameter @TODO Martin
   */
  def isEvaluable(isEvaluable: ArithExpr => Boolean = (ae) => false) = dependsOn().isEmpty 
  
  /**
   * A function which determines the variable upon which the given arithmetic expression depends 
   * 
   * @param dependsOn @TODO Martin
   * @return A set of variables 
   */
  def dependsOn(dependsOn: ArithExpr => Set[String] = (ae) => Set()) = dependsOnRek(ae,dependsOn)
  
}


/**
 * A wrapper class comprising algorithms for standard algorithmic expressions
 */
trait AE_algo {
  /**
   * Determines recursively the set of variables upon which a given arithmetic expression depends  
   * @param ae An arithmetic expression 
   * @param dependsOn A function that determines the set of variables upon which 
   * the arithmetic subexpression depends beyond the standard atomic identifier dependency 
   * @return The set of variables upon which the arithmetic expression depends 
   */
  def dependsOnRek(ae: ArithExpr, dependsOn: ArithExpr => Set[String]) :  Set[String] = ae match {  
    case UnaryArithOp(arg) => dependsOnRek(arg, dependsOn)
    case BinaryArithOp(left,right) => dependsOnRek(left, dependsOn) ++ dependsOnRek(right, dependsOn)
    case ae : ArithAtomIdentifier => Set(ae.id)
    // assuming there is another element e.g. a reference then the modify function is used for substitution 
    case sthelse => dependsOn(sthelse)
  }
  
  def substRek(ae: ArithExpr, modify: ArithExpr => ArithExpr): ArithExpr = ae match {
    case ArithMult(l, r) => ArithMult(substRek(l, modify), substRek(r, modify)) 
    case ArithDiv(l, r) => ArithDiv(substRek(l, modify), substRek(r, modify)) 
    case ArithPlus(l, r) => ArithPlus(substRek(l, modify), substRek(r, modify)) 
    case ArithMinus(l, r) => ArithMinus(substRek(l, modify), substRek(r, modify))
    case ArithExp(l, r) => ArithExp(substRek(l, modify), substRek(r, modify))
    case ArithMod(l, r) => ArithMod(substRek(l, modify), substRek(r, modify))
    // assuming there is another element e.g. a reference then the modify function is used for substitution 
    case sthelse => modify(sthelse)
  }
  
  
  def evalRek(ae: ArithExpr, modify: ArithExpr => ArithExpr): Either[Int, Double] = ae match {
    case ArithAtomValue(v) => v

    case ArithNegate(aexp) => evalRek(aexp, modify) match {
      case Right(f) => Right(-f) // Double negation
      case Left(i) => Left(-i) // integer negation
    }

    case ArithMult(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Left(il * ir) // integer multiplication

      // Double multiplication
      case (Right(l), Right(r)) => Right(l * r)
      case (Left(l), Right(r)) => Right(l * r)
      case (Right(l), Left(r)) => Right(l * r)
    }

    case ArithDiv(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Right(il / ir)
      case (Right(l), Right(r)) => Right(l / r)
      case (Left(l), Right(r)) => Right(l / r)
      case (Right(l), Left(r)) => Right(l / r)
    }

    case ArithPlus(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Left(il + ir) // integer multiplication

      // Double multiplication
      case (Right(l), Right(r)) => Right(l + r)
      case (Left(l), Right(r)) => Right(l + r)
      case (Right(l), Left(r)) => Right(l + r)
    }

    case ArithMinus(l, r) => evalRek(ArithPlus(l, ArithNegate(r)), modify)

    case ArithExp(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Left(il ^ ir) // integer multiplication

      // Double multiplication
      //case (Right(l),Right(r))=> Right(l^r)
      //case (Left(l),Right(r))=> Right(l^r)
      //case (Right(l),Left(r))=> Right(l^r)	
    }
    
    case ArithMod(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Left(il % ir) // integer multiplication
    }

    // assuming there is another element e.g. a reference then the modify function is used for substitution 
    case sthelse => {/*println("sthing else: " + sthelse + "from" + ae);*/
      val modified = modify(sthelse)
      if (modified==sthelse) throw new Exception(sthelse + " is still the same")
      evalRek(modified, modify)
    }
  }
}



trait AE_Transform[T] {
  def traverse(ae  : AE[T], modify : AE[T] => AE[T]) : AE[T] = ae match {
    case op : AE_BinaryOp[T] => modify(
        op.getClass.getConstructor(classOf[AE[T]],classOf[AE[T]]).newInstance(
            traverse(op.l,modify), traverse(op.r, modify)
        )
    )
    /*
    case AE_Plus(l,r) => modify(AE_Plus(traverse(l, modify), traverse(r, modify)))
    case AE_Min(l,r) => modify(AE_Min(traverse(l, modify), traverse(r, modify)))
    case AE_Mult(l,r) => modify(AE_Mult(traverse(l, modify), traverse(r, modify)))
    case AE_Div(l,r) => modify(AE_Div(traverse(l, modify), traverse(r, modify)))
    case AE_Mod(l,r) => modify(AE_Mod(traverse(l, modify), traverse(r, modify)))
    */
    case op : AE_UnaryOp[T] => modify(
        op.getClass.getConstructor(classOf[AE[T]]).newInstance(
            traverse(op.t,modify)
        )
    )
    /*
    case AE_Neg(v) => modify(AE_Neg(traverse(v,modify)))
    */
    case sthelse => modify(sthelse)
  }
}


class AEAlgo[T](ae: AE[T]) extends TAEAlgo {
  // using Scala 2.8 default arguments, ie. for the case that no modify function is given 
  // and expression is mapped to itself
  //def eval(modify : ArithExpr => ArithExpr = identity[ArithExpr] _) = evalRek(ae,modify)
  def eval(modify: AE[T] => AE[T] = identity[AE[T]](_)) = evalRek(ae, modify)
  def subst(modify : AE[T] => AE[T]) = substRek(ae,modify)
  def isEvaluable(isEvaluable: AE[T] => Boolean = (ae) => false) = dependsOn().isEmpty 
  def dependsOn(dependsOn: AE[T] => Set[String] = (ae) => Set()) = dependsOnRek(ae,dependsOn)
  
}


trait TAEAlgo {
  def dependsOnRek[T](ae: AE[T], dependsOn: AE[T] => Set[String]) :  Set[String] = ae match {
    case AE_BinaryOp(l,r) => dependsOnRek(l, dependsOn) ++ dependsOnRek(r, dependsOn)
    case AE_UnaryOp(t) => dependsOnRek(t, dependsOn)
    /*
    case AE_Mult(l, r) => dependsOnRek(l, dependsOn) ++ dependsOnRek(r, dependsOn)
    case AE_Div(l, r) => dependsOnRek(l, dependsOn) ++ dependsOnRek(r, dependsOn) 
    case AE_Plus(l, r) => dependsOnRek(l, dependsOn) ++ dependsOnRek(r, dependsOn) 
    case AE_Min(l, r) => dependsOnRek(l, dependsOn) ++ dependsOnRek(r, dependsOn)
    case AE_Exp(l, r) => dependsOnRek(l, dependsOn) ++ dependsOnRek(r, dependsOn)
    case AE_Mod(l, r) => dependsOnRek(l, dependsOn) ++ dependsOnRek(r, dependsOn)
    
    */
    
    //case ae : AE_Identifier => Set(ae.id)
    // assuming there is another element e.g. a reference then the modify function is used for substitution 
    case sthelse => dependsOn(sthelse)    
  }
  
  def substRek[T](ae: AE[T], modify: AE[T] => AE[T]): AE[T] = ae match {
    case op : AE_BinaryOp[T] => op.getClass.getConstructor(classOf[AE[T]],classOf[AE[T]]).newInstance(
            substRek(op.l,modify), substRek(op.r, modify)
    )
    /*
    case AE_Mult(l, r) => AE_Mult(substRek(l, modify), substRek(r, modify)) 
    case AE_Div(l, r) => AE_Div(substRek(l, modify), substRek(r, modify)) 
    case AE_Plus(l, r) => AE_Plus(substRek(l, modify), substRek(r, modify)) 
    case AE_Min(l, r) => AE_Min(substRek(l, modify), substRek(r, modify))
    case AE_Exp(l, r) => AE_Exp(substRek(l, modify), substRek(r, modify))
    case AE_Mod(l, r) => AE_Mod(substRek(l, modify), substRek(r, modify))
    */
    
    case op : AE_UnaryOp[T] => op.getClass.getConstructor(classOf[AE[T]]).newInstance(
            substRek(op.t,modify)
    )
    
    // assuming there is another element e.g. a reference then the modify function is used for substitution 
    case sthelse => modify(sthelse)
  }
  
  def evalRek[T](ae: AE[T], modify: AE[T] => AE[T]): Either[Int, Double] = ae match {

    case AE_Neg(aexp) => evalRek(aexp, modify) match {
      case Right(f) => Right(-f) // Double negation
      case Left(i) => Left(-i) // integer negation
    }

    case AE_Mult(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Left(il * ir) // integer multiplication

      // Double multiplication
      case (Right(l), Right(r)) => Right(l * r)
      case (Left(l), Right(r)) => Right(l * r)
      case (Right(l), Left(r)) => Right(l * r)
    }

    case AE_Div(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Right(il / ir)
      case (Right(l), Right(r)) => Right(l / r)
      case (Left(l), Right(r)) => Right(l / r)
      case (Right(l), Left(r)) => Right(l / r)
    }

    case AE_Plus(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Left(il + ir) // integer multiplication

      // Double multiplication
      case (Right(l), Right(r)) => Right(l + r)
      case (Left(l), Right(r)) => Right(l + r)
      case (Right(l), Left(r)) => Right(l + r)
    }
    
    case AE_Mod(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Left(il % ir)      
    }

    case AE_Min(l, r) => evalRek(AE_Plus(l, AE_Neg(r)), modify)

    case AE_Exp(l, r) => (evalRek(l, modify), evalRek(r, modify)) match {
      case (Left(il), Left(ir)) => Left(il ^ ir) // integer multiplication

      // Double multiplication
      //case (Right(l),Right(r))=> Right(l^r)
      //case (Left(l),Right(r))=> Right(l^r)
      //case (Right(l),Left(r))=> Right(l^r)	
    }
    
    // extract the value and put it into Left or Right values depending on the type of v
    case AE_Value(v) => {
      if (v.isInstanceOf[Double]) Right(v)
      else Right(v)
    }

    // assuming there is another element e.g. a reference then the modify function is used for substitution 
    case sthelse => {evalRek(modify(sthelse), modify)}    
  }
}
