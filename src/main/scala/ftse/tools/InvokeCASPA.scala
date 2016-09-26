package ftse.tools

import ftse.formalism.trace._
import ftse.formalism.spa.SPA2TextImpl
import ftse.formalism.spa.SPA_metamodel.SPA_Element
import ftse.formalism.tra.TRA_Parser
import ftse.transformations.TRA2SPAImpl
import java.io.File

/**
 * takes a CASPA SPA specification composes its concurrent processes, performs elimination and 
 * converts the resulting labeled transition system back to a CASPA specification
 * 
 * (used for stepwise elimination outside of CASPA: reduces the arising state-space while composing a model successively. Note: it's slow!)
 */
trait SPA_SPA_Eliminate extends TRA_Parser with SPA2TextImpl with TRA2SPAImpl {
  /**
   * elimination routine
   * 
   * @param elem a CASPA SPA specification
   * @return a sequential process in terms of a CASPA SPA specification, corresponding to the eliminated composed input specification 
   */
  def eliminateTau(elem: SPA_Element): SPA_Element = {
    val spatext = elem.toText

    val lines = ftse.tools.Scripting pipe (spatext, CASPA.getDirectory + "caspa -v 0 -s -1 -r -E -t stdout")
    parseTra(lines.toList).toSPA("test")
  }
}

/**
 * Deals with the command-line back-end CASPA
 * 
 * determines the operation system to address the CASPA executable right
 */
object CASPA {
  val osName = System.getProperty("os.name");
  val filename = if (osName.startsWith("Windows")) "caspa.exe" else "caspa"
  
    
  /**
   * determines the CASPA executable directory
   * 
   * if CASPA_HOME is defined and the executable exists then CASPA_HOME will be returned or  
   * else "C:\" on windows or "/usr/bin/" followed by "~/" on a UNIX-based file system will be checked 
   * for an available executable.  
   * 
   * @return an Option type that may contain a string representing the CASPA directory that provides an existing CASPA executable
   */
  def getDirectory : Option[String] = {
    val defDirs = if (osName.startsWith("Windows")) List(
	    "C:\\"
	) else List(
        "/usr/bin/",
        "~/",
		"/home/riedl/Arbeit/subversion/i41amark/dccs/tools/ReleaseCASPA/Quellen/"
	)
	  
    val validDirs = (
        (System.getenv("CASPA_HOME") :: defDirs).map(d => if (new File(d + filename).exists) Some(d) else None)
    ) collect { case Some(x) => x }
        
    validDirs.headOption
  }
  
  
  /**
   * provides the absolute path to the CASPA executable
   * 
   * @return a path to a CASPA executable that either follows a valid path given by [[ftse.tools.CASPA#getDirectory]] or addresses the local directory "./"    
   */
  def getExecutable : String = {
    getDirectory.headOption.getOrElse("./") + filename
  }
  
  
  /**
   * provides the absolute path to the CASPA executable
   * 
   * @return an Option type the may contain a valid path to a CASPA executable given by [[ftse.tools.CASPA#getDirectory]] 
   */
  def getExecutableOption : Option[String] = getDirectory.headOption.map(_ + filename) 
  
  
  /**
   * checks whether a CASPA executable has been found
   * 
   * @return 'true' if a valid path has been found or else 'false' 
   */
  def checkExecutable : Boolean = !(getDirectory.headOption==None)
  
  
  /**
   * determines the paths into a deadlock 
   * 
   * @param model a textual SPA specification
   * @return a list of traces into some deadlock  
   */
  def checkDeadlock(model : String) : List[Trace] = {
    object TP extends Trace_Parser
    
    val callstring = CASPA.getExecutable + " -r -e -1 -s -D"
    val lines = (ftse.tools.Scripting pipe(model,callstring)).toList
	
    lines.map(
	  TP.parseTrace(_)
	) collect {
      case Some(content) => content
    } 
  } 
}
