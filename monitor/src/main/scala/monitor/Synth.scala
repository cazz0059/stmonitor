package monitor

import java.io.{File, PrintWriter}

import com.typesafe.scalalogging.Logger
import monitor.interpreter.STInterpreter
import monitor.parser.STParser
import monitor.parser.STParseTree
import monitor.parser.STSolver
import monitor.parser.STSolverHelper

import scala.io.Source

class Synth {
  /**
   * Synthesises the monitor and the protocol files.
   *
   * @param path The path containing the util.scala file which also represents the directory where the monitor
   *             and protocol files are to be generated in.
   * @param fileName The name of the file containing the session type.
   * @param synthMonFile A flag to indicate whether to synthesise the monitor file or not.
   * @param synthProtocolFile A flag to indicate whether to synthesise the protocol file or not.
   */
  def apply(path: String, fileName: String, synthMonFile: Boolean, synthProtocolFile: Boolean): Unit ={
    val logger = Logger("Synth")
    val inputFile = Source.fromFile(path+"/"+fileName)
    val inputSource = inputFile.mkString

    val parser = new STParser
    parser.parseAll(parser.sessionTypeVar, inputSource) match {
      case parser.Success(r, n) =>
        println()
        logger.info("Input parsed successfully")

        val parseTree = new STParseTree(r, "original")
        try {
          parseTree.construct()
        } catch {
          case e: Exception =>
            println()
            logger.info("ParseTree Construction Error: " + e.getMessage)
            return
        }

        val solver = new STSolver(r, path)
        var rSolved = r
        try {
          rSolved = solver.run()
        } catch {
          case e: Exception =>
            println()
            logger.info("Solver Error: " + e.getMessage)
            return
        }

        val helper = new STSolverHelper()
        try {
          helper.rebuildST(rSolved)
        } catch {
          case e: Exception =>
            println()
            logger.info("ST Builder Error: " + e.getMessage)
            return
        }

        val parseTreeSolved = new STParseTree(rSolved, "solved")
        try {
          parseTreeSolved.construct()
        } catch {
          case e: Exception =>
            println()
            logger.info("ParseTree2 Error: " + e.getMessage)
            return
        }


        val interpreter = new STInterpreter(rSolved, path)
        try {
          val (mon, protocol) = interpreter.run()
          if(synthMonFile){
            lazy val monFile = new PrintWriter(new File(path+"/Mon.scala"))
            monFile.write(mon.toString)
            monFile.close()
          }
          if(synthProtocolFile){
            lazy val protocolFile = new PrintWriter(new File(path+"/CPSPc.scala"))
            protocolFile.write(protocol.toString)
            protocolFile.close()
          }
        } catch {
          case e: Exception =>
            println()
            logger.info("Interpreter Error: " + e.getMessage)
            return
        }

      case parser.Error(msg, n) =>
        println()
        logger.info("Parser Error: " + msg + " - offset: "+n.offset )
        return

      case parser.Failure(msg, n) =>
        println()
        logger.info("Parser Failure: " + msg + " - offset: "+n.offset )
        return

      case _ =>

    }
    inputFile.close()
  }
}
