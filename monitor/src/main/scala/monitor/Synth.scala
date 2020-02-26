package monitor

import java.io.{File, PrintWriter}

import com.typesafe.scalalogging.Logger
import monitor.interpreter.STInterpreter
import monitor.parser.STParser

import scala.io.Source

class Synth {
  def apply(path: String, fileName: String, synthMonFile: Boolean, synthProtocolFile: Boolean): Unit ={
    val logger = Logger("Synth")
    val inputFile = Source.fromFile(path+"/"+fileName)
    val inputSource = inputFile.mkString

    val parser = new STParser
    parser.parseAll(parser.sessionTypeVar, inputSource) match {
      case parser.Success(r, n) =>
        logger.info("Input parsed successfully")
        val interpreter = new STInterpreter(r, path)
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
