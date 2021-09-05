package dsl

import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

class InterpreterTest extends AnyFunSuite {

  test("simple program") {
    val program = """a = 'hello world'
	  b = 'cruel'
	  export a + b
	  """
    val parsed = parse(program, Parser.parseProgram(_))
    val inter = new Interpreter()
    assert(parsed.isSuccess)
    val outcome = parsed.get
    val res = inter.run(outcome.value)
    assert(res == List(Blueprint(List(LiteralGenerator(List("hello world")), LiteralGenerator(List("cruel"))))))
  }

  test("complex progam") {
    val movieNamesPath = getClass.getResource("/movie_names.txt").getPath
    val program = s"""import '${movieNamesPath}' as movies
        optPlease = ('please')?
			  screen = ('on the tv' | 'in the living room' | 'in the kitchen')?
        verb = 'play' | 'start' | 'display'
        export 'siri' +  optPlease + verb + movies + screen + optPlease
        """

    val parsed = parse(program, Parser.parseProgram(_))
    val inter = new Interpreter()
    assert(parsed.isSuccess)
    val outcome = parsed.get
    val res = inter.run(outcome.value)
    assert(
      res ==
        List(
          Blueprint(
            List(
              LiteralGenerator(List("siri")),
              LiteralGenerator(List("please", "")),
              LiteralGenerator(List("play", "start", "display")),
              LiteralGenerator(List("james bond", "harry potter", "mission impossible")),
              LiteralGenerator(List("on the tv", "in the living room", "in the kitchen", "")),
              LiteralGenerator(List("please", ""))
            )
          )
        )
    )
  }
}
