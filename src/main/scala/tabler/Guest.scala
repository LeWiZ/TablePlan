package tabler

case class Guest(val name: String,
                 val joint: String,
                 val age: Int,
                 val languages: Map[String, Double],
                 val enemies: Set[String],
                 val groups: Set[String])
{
  /**
    Returns the "language proximity" with another guest.
    1.0 : perfect (completely sharing at least one language)
    0.0 : no common language
  */
  def langageProximity(other: Guest): Double = {
    languages.keys.map(l => languages(l) * other.languages.getOrElse(l, 0.0d)).max
  }
}
