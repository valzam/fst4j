import scala.io.Source
import fst._

val a = Source.fromFile("output.txt").getLines().toList.sorted
val f = FST()
f.build(a)
f.index
f.contains("play harry potter on the tv")
f.hasPrefix("play harry pot")
