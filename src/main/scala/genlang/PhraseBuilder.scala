package genlang

object PhraseBuilder {
  def buildAsStrings(bps: Seq[Blueprint]): Seq[String] =
    bps flatMap (buildAsStrings)

  def buildAsStrings(bp: Blueprint): Seq[String] = {
    var res = Seq("")
    bp.v foreach (x => {
      x match {
        case LiteralGenerator(v) =>
          res = combine(res, v)
        case OptionalGenerator(v) =>
          v match {
            case LiteralGenerator(v) =>
              res = combine(res, v) ++ extendStringResult(res, "")
          }
      }
    })

    res.map(_.toLowerCase())
  }

  private def extendStringResult(res: Seq[String], newToken: String) =
    newToken match {
      case ""                                            => res
      case t if (t.startsWith(".") || t.startsWith(",")) => res.map(os => os ++ newToken)
      case _ =>
        res.map(os => os ++ " " ++ newToken.trim)
    }

  private def combine(base: Seq[String], s: Seq[String]) =
    s.foldLeft(List[String]())((lh, s) => lh ++ extendStringResult(base, s))
}
