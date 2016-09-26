package ftse.tools

/**
 * Provides a common methods for logging 
 */
trait Logger {
  var indent = 0
  var l: String = initial
  val mem = new scala.collection.mutable.HashMap[String, String]()

  /**
   * Initial text string when starting logging
   */
  def initial: String
  
  /**
   * Final text string when retrieving a log  
   */
  def finish: String
  
  /** 
   * Methods to further indent  
   */
  def indentInkr = indent += 2
  
  /**
   * Methods to revert last indent
   */
  def indentDekr = if (indent > 1) indent -= 2 else indent = 0

  /**
   * Add a log 
   * 
   * @param s A log
   */
  def log(s: String) : Unit = {
    val message = " " * indent + s.replaceAll("\n", "\n" + " " * indent)
    //println(message) // live logging
    l += message + "\n"
  }
  
  /** Add a log and then indent */
  def logInkr(s: String) = { log(s); indentInkr; }
  
  /** Indent and then add a log */
  def inkrLog(s: String) = { indentInkr; log(s); }
  
  /** Revert the last indent and then add a log */
  def dekrLog(s: String) = { indentDekr; log(s); }
  
  /** Add a log and then revert the last indent */
  def logDekr(s: String) = { log(s); indentDekr; }

  /** Clean the log and restart with initial log text */
  def clean = l = initial

  def memorize(id: String) = mem(id) = (l + finish)
  def getMemEntries = mem.keys
  def getFromMem(id: String) = mem(id)
  def getLog: String = initial + l + finish
  override def toString: String = getLog
}

/**
 * Very basic logger to produce Dot-Graphs 
 */
trait DotLogger extends Logger {
  val initial = "digraph G {\n  ratio=1.3;\n"
  val finish = "}"
}

/**
 * Logger to produce PetriNets 
 */
trait PetriNetLogger extends DotLogger {
  val entries = scala.collection.mutable.ArrayBuffer[(String, String)]()

  /**
   * add a place
   */
  def addPlace(subgraph: String, idPlace: String) = {
    val dotPlace = {
      idPlace + "[]"
    }
    entries.+=((subgraph, dotPlace))
  }

  /**
   * add a transition
   */
  def addTransition(subgraph: String, idTransition: String, idSourcePlaces: List[String], distr: Option[String] = None, idTargetPlaces: List[String], ef: Option[String] = None) = {
    val dotTransition = {
      idSourcePlaces.map(idSourcePlace => idSourcePlace + " -> " + idTransition).mkString("", ";", ";") +
        idTransition + " [" + (if (!ef.isDefined) "" else "style=filled label=\"" + ef.get + "\"") + (if (distr.isDefined) " color = lightgrey " else " fontsize=7 fontcolor=white color=black") + " shape=box]" + "; " +
        idTargetPlaces.map(idTargetPlace => idTransition + " -> " + idTargetPlace).mkString("", ";", ";")
    }
    entries.+=((subgraph, dotTransition))
  }

  /**
   * Retrieve the dot representation
   */
  def getDot = {
    val groupedEntries = entries.groupBy(_._1)
    val subgraphentries = groupedEntries.filter(a => a._1 != "" | a._1 != "root" | a._1 != "main")
    val rootentries = groupedEntries.filter(a => a._1 == "" | a._1 == "root" | a._1 == "main")
    val root = initial 
    val all = subgraphentries.map(a => 
      a._2.foldLeft("subgraph " + "cluster_" + a._1 + " {\n    label=\"" + a._1 + "\";")((a, b) => a + "\n    " + b._2) + "\n  }").foldLeft(root)((a, b) => a + "\n\n  " + b)

    all + "\n" + finish
  }
}

/**
 * TODO: comment CompositionLogger
 */
object CompositionLogger extends Logger {
  def initial = "digraph G {\n  ratio=1.3;\n"
  def finish = "\n}"
  def getComposition = l + finish
}

/**
 * TODO: comment BddLogger
 */
object BddLogger extends Logger {
  def initial = "digraph G {\n  ratio=1.3;\n"
  def finish = "\n}"
  def getDot = l + finish
}