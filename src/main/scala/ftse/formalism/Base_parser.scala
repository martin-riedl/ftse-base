package ftse.formalism

import scala.util.parsing.combinator._

/**
 * Treats Comments such as /* ... */ and // as white-spaces
 */
trait ParseComments extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
}
