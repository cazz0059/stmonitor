package monitor.synth

import com.typesafe.scalalogging.Logger
import monitor.interpreter.STInterpreter
import monitor.parser.STParser
import monitor.parser.STParseTree
import monitor.parser.STSolver
import monitor.parser.STSolverHelper

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.Try

class Synth {
  /**
   * Synthesises the monitor and the protocol files.
   *
   * @param directoryPath The path containing the util.scala file which also represents the directory where the monitor and protocol files are to be generated in.
   * @param sessionTypePath The path of the file containing the session type.
   * @param preamblePath The path to the file containing the preamble of the monitor code.
   * @param synthMonFile A flag to indicate whether to synthesise the monitor file or not.
   * @param synthProtocolFile A flag to indicate whether to synthesise the protocol file or not.
   */
  def apply(directoryPath: String, sessionTypePath: String, preamblePath: String, synthMonFile: Boolean, synthProtocolFile: Boolean): Unit ={
    val logger = Logger("Synth")
    val inputFile = Source.fromFile(sessionTypePath)
    val inputSource = inputFile.mkString

    val parser = new STParser
    parser.parseAll(parser.sessionTypeVar, inputSource) match {
      case parser.Success(r, n) =>
        val stFile = sessionTypePath.substring(sessionTypePath.lastIndexOf('/')+1)
        logger.info(f"Input type $stFile parsed successfully")
        val preambleFile = Try(Source.fromFile(preamblePath).mkString)

        val parseTree = new STParseTree(r, "original")
        try {
          parseTree.construct()
        } catch {
          case e: Exception =>
            println()
            logger.info("ParseTree Construction Error: " + e.getMessage)
            return
        }

        val solver = new STSolver(r, directoryPath, preambleFile.getOrElse(""))
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


        val interpreter = new STInterpreter(r, directoryPath, preambleFile.getOrElse(""))
        val interpreterOpt = new STInterpreter(rSolved, directoryPath, preambleFile.getOrElse(""))
        try {
          logger.info("Running interpreter")
          val (mon, protocol) = interpreter.run()
          logger.info("Running optimised interpreter")
          val (monOpt, protocolOpt) = interpreterOpt.run()
          logger.info("Both interpreters run")
          if(synthMonFile){
            lazy val monFile = new PrintWriter(new File(directoryPath+"/Monitor-Original.txt"))
            monFile.write(mon.toString)
            monFile.close()

            lazy val monOptFile = new PrintWriter(new File(directoryPath+"/Monitor.scala"))
            monOptFile.write(monOpt.toString)
            monOptFile.close()
          }
          if(synthProtocolFile){
            lazy val protocolFile = new PrintWriter(new File(directoryPath+"/CPSPc-Original.txt"))
            protocolFile.write(protocol.toString)
            protocolFile.close()

            lazy val protocolOptFile = new PrintWriter(new File(directoryPath+"/CPSPc.scala"))
            protocolOptFile.write(protocolOpt.toString)
            protocolOptFile.close()
          }
          logger.info(f"Successful synthesis for input type $stFile at $directoryPath")
        } catch {
          case e: Exception =>
            println("Error: " + e.getMessage)
        }

      case parser.Error(msg, n) =>
        println("Parser Error: " + msg + " offset: "+n.offset )

      case parser.Failure(msg, n) =>
        println("Parser Error: " + msg + " offset: "+n.offset )

      case _ =>

    }
    inputFile.close()
  }
}
