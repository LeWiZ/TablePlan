package tabler

object Logger {
  def apply(l: Int, s: String) = {
    if (l <= 1) println("### " + s)
    else if (l == 2) println(" ## " + s)
    else println("  #" + s)
  }

  def warning(s: String) = println("  !" + s)
  def error(s: String) = {
    println("!!! " + s)
    sys.exit(0)
  }
}
