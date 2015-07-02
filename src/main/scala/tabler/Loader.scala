package tabler

object Loader {

  class CSV(url: String) {

    val csv: Seq[Seq[String]] = scala.io.Source.fromURL(url).mkString.lines.map(_.split(",").toSeq).toSeq

    lazy val height: Int = csv.size
    lazy val width: Int = csv.map(_.size).max

    def apply(x: Int, y: Int): String = {
      if (x < 0 || x >= width || y < 0 || y >= height) ""
      else if (x >= csv(y).size) ""
      else csv(y)(x)
    }

    def columnFrom(x: Int, y: Int): Seq[String] = for (j <- y until height ; c = this(x, j) if c != "") yield c

  }

  object CSV {
    def apply(url: String) = new CSV(url)
  }

  def apply(url: String): (Set[Guest], Set[Table]) = {

    // Download the csv file & extract all cells
    val csv: CSV = CSV(url)

    // Get the list of possible groups
    val possGroups: Set[String] = csv.columnFrom(9, 1).toSet

    // Get the list of table sizes
    val tableSizes: Seq[Int] = csv.columnFrom(8,1) map (_.toInt)

    // Get the list of included guests with associated properties
    val inclusions: Seq[Boolean] = csv.columnFrom(0,1).map(_ != "")
    val names: Seq[String] = csv.columnFrom(1,1)
    val joints: Seq[String] = csv.columnFrom(2,1)
    val ages: Seq[Int] = csv.columnFrom(3,1).map(_.toInt)


    (Set[Guest](), Set[Table]())

  }

}
