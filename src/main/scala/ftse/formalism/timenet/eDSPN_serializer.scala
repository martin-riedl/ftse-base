package ftse.formalism.timenet

/**
 * Provides implicit methods to an EDSPN (see [[eDSPN_serializer]])
 */
trait eDSPN_serializer_impl {
  implicit def serialize(net: EDSPNNet) = new eDSPN_serializer(net)
}

/**
 * EDSPN Serializer
 * 
 * @param net Input EDSPN
 */
class eDSPN_serializer(net: EDSPNNet) extends eDSPN_dot_serializer with toSPNP {
  /** Serializes the EDSPN to its DOT representation */
  def toDot: String = { toDot(net); getDot }
  
  /** Serializes the EDSPN to its text representation */
  def toSPNP: String = toSPNP(net)
}

/**
 * EDSPN DOT Serializer
 */
trait eDSPN_dot_serializer extends ftse.tools.PetriNetLogger {
  protected def toDot(net: EDSPNNet) = {

    val fromNodeArcs = net.arc.groupBy(a => a.fromNode)
    val toNodeArcs = net.arc.groupBy(a => a.toNode)

    net.place.foreach(p => {
      addPlace("", p.id)
    })

    net.exponentialTransition.foreach(et => {
      val toPlaces = fromNodeArcs(et.id).map(p => p.toNode).toList
      val fromPlaces = toNodeArcs(et.id).map(p => p.fromNode).toList

      addTransition("", et.id, toPlaces, Some(et.delay), toPlaces, None)
    })

    net.immediateTransition.foreach(it => {
      val toPlaces = fromNodeArcs(it.id).map(p => p.toNode).toList
      val fromPlaces = toNodeArcs(it.id).map(p => p.fromNode).toList

      addTransition("", it.id, toPlaces, None, toPlaces, Some(it.enablingFunction))
    })
  }
}

/**
 * EDSPN Text Serializer
 */
trait toSPNP {
  protected def convertEnablingFunction(ef: String) = {
    import scala.util.matching.Regex

    val Atom = new Regex("""#(\w+)""")
    val OR = new Regex("""OR""")
    val AND = new Regex("""AND""")
    val NOT = new Regex("""NOT""")
    val EQUALS = new Regex("""=""")

    List(
      (str: String) => Atom.replaceAllIn(str, """mark($1)"""),
      (str: String) => OR.replaceAllIn(str, """||"""),
      (str: String) => AND.replaceAllIn(str, """&&"""),
      (str: String) => NOT.replaceAllIn(str, """!"""),
      (str: String) => EQUALS.replaceAllIn(str, """==""")).foldLeft(ef)((a, b) => b(a))
  }

  protected def toSPNP(net: EDSPNNet) = {
    val fromNodeArcs = net.arc.groupBy(a => a.fromNode)
    val toNodeArcs = net.arc.groupBy(a => a.toNode)

    val places = net.place.map(p => {
      "  place(\"" + p.id + "\");"
    })

    val immediates = net.immediateTransition.map(it => {
      val toPlaces = fromNodeArcs(it.id).map(p => p.toNode)
      val fromPlaces = toNodeArcs(it.id).map(p => p.fromNode)

      "  imm(\"" + it.id + "\");\n" +
        "  int enablingfunc_" + it.id + "() {\n" +
        "    " + convertEnablingFunction(it.enablingFunction) + "\n" +
        "  }\n" +
        "  guard(\"" + it.id + "\",enablingfunc_" + it.id + "());\n" +
        fromPlaces.map(i => "  iarc(\"" + it.id + "\",\"" + i + "\");").mkString("", "\n", "\n") +
        toPlaces.map(o => "  iarc(\"" + it.id + "\",\"" + o + "\");").mkString("", "\n", "\n")
    })

    val exponentials = net.exponentialTransition.map(et => {
      val toPlaces = fromNodeArcs(et.id).map(p => p.toNode)
      val fromPlaces = toNodeArcs(et.id).map(p => p.fromNode)

      "  void rateval(\"" + et.id + "\"," + et.delay + ");\n" +
        fromPlaces.map(i => "  iarc(\"" + et.id + "\",\"" + i + "\");").mkString("", "\n", "\n") +
        toPlaces.map(o => "  iarc(\"" + et.id + "\",\"" + o + "\");").mkString("", "\n", "\n")
    })

    val cspec =
      exponentials.foldLeft(
        immediates.foldLeft(
          places.foldLeft("void net() {")((a, b) => a + "\n" + b))((a, b) => a + "\n\n" + b))((a, b) => a + "\n\n" + b)
    cspec + "}"
  }
}
