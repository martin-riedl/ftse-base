package ftse.formalism.spa

import SPA_metamodel._
import ftse.formalism.arith._
import ftse.formalism.logical._
import ftse.formalism.arith.ArithExprSerializerImpl

class SPA_Generator(elem: SPA_Element) extends SPA2Text//  with SPA_SPA_Eliminate
{
  def toText: String = toText(elem, 0)
  //def eliminateTau : SPA_Element =  eliminateTau(elem)
}

trait SPA2TextImpl {
  implicit def transform(e: SPA_Element) = new SPA_Generator(e)
}

/**
 * serialisation module using pattern matching and views
 */
trait SPA2Text  extends  ArithExprSerializerImpl with  LogicExpr2Text {
  import LE_metamodel._
  
  override def toText(elem: LExpAtom): String = elem match {
    //case AtomStr(value) => value
    case SMDefAtom(process, optInt) => toText(process, 0) + optInt.getOrElse("")
  }
  
  override val not = "!"
  
  def toText(e: SPA_Element, depth: Int): String = e match {
    case SPASpec(defs) => (for (d <- defs) yield toText(d, depth + 2)).mkString("", "", "")
    case ProcessDefinition(name, parameters, gps) => {
      "\n\n" +
        name + (if (parameters.length > 0) (for (p <- parameters) yield toText(p, depth + 2)).mkString("(", ",", ")")
        else "") + " := " +
        (for (g <- gps.toList.sortWith((g1,g2)=>g1.toString<g2.toString)) yield toText(g, depth + 2)).mkString("")
    }
    case GuardedProcess(guard, p) => "\n" + " " * depth + toText(guard, depth + 2) + " -> " + toText(p, depth + 2)
    case ParallelComposition(p1, p2, actionlabels) => {
      "\n" + " " * depth + "(" + (p1 match {
        case proc_instance : I => "\n" + " " * depth + "  " + toText(proc_instance, depth + 2)
        case p => toText(p, depth + 2)
      }) +
        "\n" + " " * depth + actionlabels.toList.sortWith((a,b)=>a<b).mkString("  |[", ",", "]|") +
        (p2 match {
          case proc_instance: I => "\n" + " " * depth + "  " + toText(proc_instance, depth + 2)
          case p => toText(p, depth + 2)
        }) +
        "\n" + " " * depth + ")"
    }
    case Choice(ps1, ps2) => {
      "\n" + " " * depth + "(" + toText(ps1, depth + 2) +
        "\n" + " " * depth + "  + " + toText(ps2, depth + 2) +
        "\n" + " " * depth + ")"
    }
    case Stop => "stop"
    case Hiding(actions, process) => {
      "\n" + " " * depth + "hide" + (for (a <- actions) yield a).mkString(" ", ",", " ") + "in " + "(" +
        toText(process, depth + 2) +
        "\n" + " " * depth + ")"
    }
    case Prefix(a, fb) => toText(a, depth + 2) + ";" + toText(fb, depth + 2)
    //case MA(label,r) => "(" + label + "," + {import java.util.Locale;import java.text._;val nf = new DecimalFormat("###0.00######################", new DecimalFormatSymbols(Locale.US)); nf.format(r)} + ")"
    case MA(label, r) => "(" + label + "," + r.toText + ")"
    case IA(label, r) => "(*" + label + "," + r.toText + "*)"
    case I(ref, initvector) => ref + (if (initvector.isEmpty) "" else initvector.map(_.toText).mkString("(", ",", ")"))
    //case ProcessIntfDefinition(name, parameters) => name + parameters.mkString("(", ",", ")")
    case ProcRef(process, Some(condlist)) => process + "(" + (if (condlist.isEmpty) "*" else condlist.map(a => toText(a,0)).mkString("",",","")) + ")"
    case ProcRef(process, None) => process 
    case PPD(name, max) => name + " [" + (if (max.isLeft) max.left.get else max.right.get) + "]"
    case Guard(List()) => "[*]"
    case Guard(g) => "[" + g.map(c => toText(c,depth+2)).mkString("",",","") + "]"
    case Condition(lhs,op,rhs) => lhs.toText + op + rhs.toText   
    case AllCondition => "*"
    case ConstantDefinition(name, arithexpr) => "\nint " + name + " = " + arithexpr.toText + ";"
    case RateBinding(name, value) => "\nrate " + name + " = " + (if (value.isLeft) value.left.get else value.right.get) + ";"
    case WeightBinding(name, value) => "\nweight " + name + " = " + (if (value.isLeft) value.left.get else value.right.get) + ";"
    case StateMeasure(variable,sm) => "\nstatemeasure " + variable + " " + toText(sm)
    case ThroughputMeasure(variable,actionLabel) => "\nthroughputmeasure " + variable + " " + actionLabel 
    case MeanValue(variable, refProcess, refParameter, optNumber) => "\nmeanvalue " + variable + " "+ refProcess + " "+refParameter + " " + optNumber
    case e => throw new Exception(e.toString) //new SPAPatternMatchException("Parse error",e)
  }
}

class SPAPatternMatchException(str: String, e: Any) extends java.lang.Throwable
