package ftse.formalism.trace
import scala.util.parsing.combinator._

/**
 * Trace 
 * 
 * @param events A sequence of strings representing the event labels
 */
case class Trace(events : List[String])


/**
 * Trace Parser Combinator
 */
trait Trace_Parser extends JavaTokenParsers {
  
  
  /**
   * Parses a trace 
   * 
   * @param v String that represents a trace
   * @return An Option type that may contain a parsed trace 
   */
  def parseTrace(v : String) : Option[Trace] = {
    val s = v.replace("Initial State", "Initial")
    val res = parse(Trace_PC,s)
    if (res.successful)
    	Some(Trace(res.get.reverse))
    else 
    	None
  }
  
  /**
   * Trace parsing function
   */
  private def Trace_PC: Parser[List[String]] =
    rep1sep(ident, "<-") /*~ "<-" ~ "Initial State"*/ ^^ {
    	case all /*~ _ ~ i*/ => all //:+ i
    } 
}

/**
 * Trace Serializer
 */
trait TraceSerializer {

  
  /**
   * Converts a trace to its textual representation 
   * 
   * @param t A trace
   * @return A textual representation of the given trace
   */
  def toTrace(t : Trace) : String =  {
	t.events.map(_ match {
	  case sth => sth
	}).mkString ("->")
  }
  
  
  /**
   * Serializes a trace to a textual DOT specification 
   * 
   * @param t A trace
   * @return A textual DOT representation of the given trace
   */
  def toDot(traces: List[Trace]): String = {
    val content = traces.zipWithIndex map { tuple =>
      val (t, tracenum) = tuple
    	val tail = t.events.tail.zipWithIndex.map(e => (e._1,e._2+1))
	    // TODO: tracenum einfügen
    	tail.foldLeft(List("node [style=filled]",
    	    "\"" + tracenum + "_"+"0"+ "\"" + " [shape=box, label=Initial, fillcolor=green]"))((a,b) => {
	      val nextNode = 
	        "\"" + tracenum + "_" + b._2 + "\"" + " ["+ (
		      if (b==tail.last) 
		        "label=Deadlock, shape=box, fillcolor=red" 
		      else 
		        "label=\"\", shape=point, fillcolor=blue"
	      ) + "]"
	      val transition = 
		      "\"" + tracenum + "_" + (b._2-1) + "\"" + 
		      "->" + 
		      "\"" + tracenum + "_" + b._2 + "\"" + " [label="+b._1+"]"
	      a :+ nextNode :+ transition
	    })
    }
    
    content.flatten.mkString("digraph agent {\n  rankdir=LR\n  ","\n  ","\n}")
  }
}
