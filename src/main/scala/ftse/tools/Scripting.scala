package ftse.tools

import scala.io.Source
import java.io._
import sys.process._
import ftse.formalism.tra._

/**
 * simplifies the usage of command line pipes and batches to define workflows 
 */
object Scripting {
  
  /**
   * pipe a string to a shell command and collect the output 
   * 
   * @param input string to be piped
   * @param cmd to be executed by the shell 
   * @return the output lines
   */ 
  def pipe(input: String, cmd: String): Iterator[String] = {
    val inputStream = new ByteArrayInputStream(input.getBytes())
    ((cmd #< inputStream) lines_!) iterator 
  }

  
  /**
   * pipe a string to a shell command
   * 
   * @param input string to be piped
   * @param cmd to be executed by the shell 
   */
  def pipeIn(input: String, cmd: String): Unit = {
    val inputStream = new ByteArrayInputStream(input.getBytes())
    (cmd #< inputStream) !
  }
  
  
  /**
   * execute a Unix shell command and return its result code
   * 
   * @param cmd shell command
   * @return result code 
   */
  def exitValue(cmd: String): Int = {
    cmd !
  }
  
  
  /**
   * reads all lines contained by a file
   * 
   * @param file path to the file
   * @return sequence of lines
   */
  def readFile(file : String) : Iterator[String] =  {
    Source.fromFile(file, "ISO8859_1").getLines
  }
  
  /**
   * pipes an input text to a shell command without blocking
   * 
   * @param input text to be piped
   * @param destProgramm shell command to be executed
   * @param a process
   */
  def pipeInputTo(input : String, destProgram : String) : Process = {
	  val is = new ByteArrayInputStream(input.getBytes)
	  "cat" #< is #| destProgram run //! 
  }
  
  
  /**
   * reads a file in a single string
   * 
   * @param file path referring to the input file 
   * @return a single string, i.e. the content of the file
   */
  def readFileAsString(file : String) : String = readFile(file).foldLeft("")(_+ "\n" +_)

  
  /**
   * visualizes a labeled transitions system via a DOT/GraphViz representation using [[ftse.tools.Scripting#showDot]]
   * @param tra labeled transition system
   */
  def showTra(tra : Tra) = {
      object Serializer extends TraSerializer
	  val dot = Serializer.toDot(tra)
	  showDot(dot)
  }
  
  
  /**
   * visualizes a textual dot representation using GraphViz and AcroRead
   * 
   * the 'dot' executable is either addressed by a DOTTY_HOME variable set or by default '/usr/bin/' 
   * for the 'acroread' executable it is assumed to be in the PATH variable      
   * 
   * @param dot textual DOT graph representation 
   */
  def showDot(dot: String) = {
    val inputStream = new ByteArrayInputStream(dot.getBytes())
    val dotty_home = Option(System.getenv("DOTTY_HOME")).getOrElse("/usr/bin/")
    val acro_home = Option(System.getenv("ACRO_HOME")).getOrElse("/usr/bin/")
    if ((new File(acro_home,"acrord32.exe")).exists()) {
      //((dotty_home + "dot -Tpdf" ) #< inputStream) #>> new File("tmp.pdf") !;
      //(acro_home+"acrord32.exe tmp.pdf") run; 
      ((dotty_home + "dotty" ) #< inputStream) run; 
    } else {
      ((dotty_home + "dot -Tpdf" ) #< inputStream) #| ("acroread -") run
    }
    
    //val rsvgview_home = Option(System.getenv("RSVGVIEW_HOME")).getOrElse("/usr/bin/")
    
    //((dotty_home + "dot -Tsvg") #< inputStream) #| (rsvgview_home + "rsvg-view-3 -w 1000 -k --stdin") !
    //((dotty_home + "dot -Tpdf" ) #< inputStream) #| ("acroread -") !
    
   // (("cat" ) #< inputStream) #| ("dotty -") !
  }
}
