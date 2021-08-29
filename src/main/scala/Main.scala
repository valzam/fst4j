package genlang

import fastparse._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import scala.io.Source

import java.io._

object Main extends App {

  val fileContents = Source.fromFile("input.txt").getLines().mkString("\n")
  val int = new Interpreter()
  parse(fileContents, Parser.parseProgram(_))
  val res = parse(fileContents, Parser.parseProgram(_)) match {
    case f: Failure => {
      println(f)
      throw new Error("Parsing failure")
    }
    case Success(value, index) => int.run(value)
  }

  val pw = new PrintWriter(new File("output.txt"))
  pw.write(PhraseBuilder.buildAsStrings(res).mkString("\n"))
  pw.close()
}
