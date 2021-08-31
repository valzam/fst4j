package dsl

import java.lang.Thread.State
import fastparse._
import SingleLineWhitespace._

sealed trait BaseToken extends Serializable with Product
sealed trait Expr extends BaseToken

case class StrLiteral(value: String) extends Expr
case class Variable(value: String) extends Expr
case class OrCombinator(options: Seq[Expr]) extends Expr
case class OptionalCombinator(content: Expr) extends Expr

sealed trait Statement extends BaseToken
case class VariableDeclStat(name: Variable, value: Seq[Expr]) extends Statement
case class ImportStat(filePath: StrLiteral, name: Variable) extends Statement
case class ExportStat(value: Seq[Expr]) extends Statement

object Parser {
  def chainer[_: P] = P("+".?)
  def string[_: P] =
    P("'" ~ CharIn("a-zA-Z?!,.-_").rep(1).! ~ "'" ~ chainer).map(StrLiteral)
  def variableName[_: P] =
    P(CharsWhileIn("a-zA-Z").rep(1).! ~ chainer).map(Variable)
  def orCombinator[_: P] =
    P((string | variableName) ~ ("|" ~ (string | variableName)).rep(1) ~ chainer).map(x =>
      OrCombinator(List(x._1) ++ x._2)
    )
  def optionalString[_: P] =
    P("(" ~ (orCombinator | string) ~ ")?" ~ chainer).map(OptionalCombinator)

  def expr[_: P] = P(optionalString | orCombinator | string | variableName)

  def variableDeclStatement[_: P] =
    P(variableName ~ "=" ~ (expr).rep).map(x => VariableDeclStat(x._1, x._2))
  def importStat[_: P] =
    P("import" ~ string ~ "as" ~ variableName).map(x => ImportStat(x._1, x._2))
  def exportStatement[_: P] = P("export" ~ expr.rep).map(ExportStat)

  def parseStatement[_: P] = P((exportStatement | importStat | variableDeclStatement) ~ "\n".rep().?)
  def parseProgram[_: P] = P((parseStatement).rep ~ End)
}
