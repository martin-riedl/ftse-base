package ftse.formalism.tra

/**
 * Tra Serializer
 */
trait TraSerializer {

  /**
   * Serializes a transition system 
   * 
   * @param tra A transition system 
   * @return The transition system in textual TRA format 
   */
  def toTra(tra : Tra) : String =  {
	tra.transitions.flatMap(s => s._2.map(_ match {
      case t: MarkovianTransition =>
        t.label  + " " + t.source + " " + t.target +" " + t.value +" M"
      case t: ImmediateTransition =>
        t.label  + " " + t.source + " " + t.target +" " + t.value +" I"
    })) mkString ("", "\n", "\n")
  }
  
  /**
   * Serializes a transition system to a textual DOT representation
   * 
   * @param tra A transition system 
   * @return The transition system in textual DOT format
   */
  def toDot(tra: Tra): String = {
    val tstr = tra.transitions.flatMap(s => s._2.map(_ match {
      case t: MarkovianTransition =>
        "  " + t.source + "->" + t.target + "[fontsize=15, label=\"(" + t.label+","+t.value + ")\", style=solid];"
      case t: ImmediateTransition =>
        "  " + t.source + "->" + t.target + "[fontsize=15, label=\"(" + t.label+","+t.value + ")\", style=dashed];"
    })) mkString ("", "\n", "\n")
    "digraph G {\n  ratio=1.3;\n" + tstr + "}"
  }  
}