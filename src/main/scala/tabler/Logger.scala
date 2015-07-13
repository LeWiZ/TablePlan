package tabler

object Logger {
  def log1(s: String) = println("### " + s)
  def log2(s: String) = println(" ## " + s)
  def log3(s: String) = println("  # " + s)

  def warning(s: String) = println("  !" + s)
  def error(s: String) = {
    println("!!! " + s)
    sys.exit(0)
  }
}
