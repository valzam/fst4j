package dsl

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

class ParserTest extends AnyFunSuite {
  test("parses empty programs") {
    val progam = """"""
    val res = parse(progam, Parser.parseProgram(_))

    assert(res.isSuccess)
  }

  test("creates variables from string literals") {
    val progam = """a = 'hello world'"""
    val res = parse(progam, Parser.parseProgram(_))

    assert(res.isSuccess)
    val outcome = res.get
    assert(outcome.value === List(VariableDeclStat(Variable("a"), List(StrLiteral("hello world")))))
  }

  test("creates variables from combinations") {
    val progam = """a = 'hello world' | 'hello wide world'"""
    val res = parse(progam, Parser.parseProgram(_))

    assert(res.isSuccess)
    val outcome = res.get
    assert(
      outcome.value === List(
        VariableDeclStat(
          Variable("a"),
          List(OrCombinator(List(StrLiteral("hello world"), StrLiteral("hello wide world"))))
        )
      )
    )
  }

  test("creates variables with optional values") {
    val progam = """a = ('hello world' | 'hello wide world')?
                   |b = ('cruel')?""".stripMargin
    val res = parse(progam, Parser.parseProgram(_))

    assert(res.isSuccess)
    val outcome = res.get
    assert(
      outcome.value === List(
        VariableDeclStat(
          Variable("a"),
          List(
            OptionalCombinator(OrCombinator(List(StrLiteral("hello world"), StrLiteral("hello wide world"))))
          )
        ),
        VariableDeclStat(
          Variable("b"),
          List(
            OptionalCombinator(StrLiteral("cruel"))
          )
        )
      )
    )
  }

  test("simple program") {
    val progam = """a = 'hello world'
	  b = 'cruel'
	  export a + b
	  """
    val res = parse(progam, Parser.parseProgram(_))

    assert(res.isSuccess)
    val outcome = res.get

    assert(
      outcome.value === List(
        VariableDeclStat(Variable("a"), List(StrLiteral("hello world"))),
        VariableDeclStat(Variable("b"), List(StrLiteral("cruel"))),
        ExportStat(List(Variable("a"), Variable("b")))
      )
    )
  }

  test("complex program") {
    val progam = """a = 'world' | 'moon'
	  b = ('cruel')?
	  c = ('how are you' | 'how was your day')?
	  export 'hello' + b + a + c
	  """
    val res = parse(progam, Parser.parseProgram(_))

    assert(res.isSuccess)
    val outcome = res.get

    assert(
      outcome.value === List(
        VariableDeclStat(Variable("a"), List(OrCombinator(List(StrLiteral("world"), StrLiteral("moon"))))),
        VariableDeclStat(Variable("b"), List(OptionalCombinator(StrLiteral("cruel")))),
        VariableDeclStat(
          Variable("c"),
          List(OptionalCombinator(OrCombinator(List(StrLiteral("how are you"), StrLiteral("how was your day")))))
        ),
        ExportStat(List(StrLiteral("hello"), Variable("b"), Variable("a"), Variable("c")))
      )
    )
  }
}
