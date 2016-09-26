package ftse.tools

object Helper {

  /* Extractor Object for Parsing an Integer from a String */
  object Int {
    def unapply(s : String) : Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }
  
}