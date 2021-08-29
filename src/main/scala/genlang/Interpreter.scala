package genlang

import scala.collection.mutable
import scala.io.Source

sealed trait Generator extends Product with Serializable
sealed trait MaybeOptional extends Product with Serializable
case class LiteralGenerator(v: Seq[String]) extends Generator with MaybeOptional
case class OptionalGenerator(v: MaybeOptional) extends Generator

case class Blueprint(v: Seq[Generator])

class Interpreter {
  val heap = mutable.Map[String, Generator]()

  def run(p: Seq[Statement]): Seq[Blueprint] = {
    var exports = Seq[Blueprint]()
    p foreach (s => {
      s match {
        case ExportStat(expressions) =>
          val result = evaluateExprs(expressions)
          exports = exports :+ result
        case VariableDeclStat(name, expressions) =>
          val value = evaluateExprs(expressions)
          val reifiedExpr = PhraseBuilder.buildAsStrings(value)
          heap.addOne(name.value -> LiteralGenerator(reifiedExpr))
        case ImportStat(filePath, name) =>
          val fileContents = Source.fromFile(filePath.value).getLines().toList
          heap.addOne(name.value -> LiteralGenerator(fileContents))
      }
    })

    exports
  }

  def evaluateExprs(e: Seq[Expr]): Blueprint = {
    val res = e.map(evaluateExpr(_))
    Blueprint(res)
  }

  def evaluateExpr(e: Expr): Generator = e match {
    case StrLiteral(value) => LiteralGenerator(Seq(value))
    case Variable(value)   => heap(value)
    case OrCombinator(values) => {
      val res = evaluateExprs(values).v.flatMap(x => {
        x match {
          case LiteralGenerator(v) => v
          case _ =>
            throw new Error("Invalid content for combinator")
        }
      })
      LiteralGenerator(res)
    }
    case OptionalCombinator(content) => {
      evaluateExpr(content) match {
        case LiteralGenerator(v) =>
          OptionalGenerator(LiteralGenerator(v))
        case _ => throw new Error("Invalid content for optional")
      }
    }
  }

}
