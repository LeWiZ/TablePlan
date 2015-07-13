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

    def allFrom(x: Int, y: Int): Seq[String] = for (j <- y until height ; c = this(x, j)) yield c
    def nonEmptyFrom(x: Int, y: Int): Seq[String] = for (j <- y until height ; c = this(x, j) if c != "") yield c

  }

  object CSV {
    def apply(url: String) = new CSV(url)
  }

  def apply(url: String): (Set[Guest], Set[Table]) = {

    // Download the csv file & extract all cells
    val csv: CSV = CSV(url)

    // Get the list of possible groups
    val possGroups: Set[String] = csv.nonEmptyFrom(9, 1).toSet

    // Get the list of possible languages
    val possLanguages: Set[String] = csv.nonEmptyFrom(10,1).toSet

    // Get the list of table sizes
    val tableSizes: Seq[Int] = csv.nonEmptyFrom(8,1) map (_.toInt)

    // Get the list of included guests
    val inclusions: Seq[Boolean] = csv.allFrom(0,1).map(_ != "")

    // Get the list of guests' name
    val names: Seq[String] = csv.allFrom(1,1)

    // Get the list of guests' joint
    val joints: Seq[String] = csv.allFrom(2,1)
    val nj: Option[String] = joints.filter(_ != "").find(j => !names.contains(j))
    if (nj.nonEmpty) Logger.error("Non-existant joint name : " + nj.get)

    // Get the list of guests' age
    val ages: Seq[Int] = csv.allFrom(3,1).map(_.toInt)

    // Get the list of guests' languages
    val languages: Seq[Map[String, Double]] = {
      csv.allFrom(4,1) map { s =>
        if (s == "") Map[String, Double]()
        else {
          s.split(' ')
          .toSeq
          .map(s => s.split('('))
          .map(s => (s(0),s(1).dropRight(1).toDouble / 100.0d))
          .toMap
        }
      }
    }
    val nl: Option[String] = languages.map(_.keys).flatten.filter(_ != "").find(l => !possLanguages.contains(l))
    if (nl.nonEmpty) Logger.error("Non-existant language name : " + nl.get)

    // Get the list of guests' enemies
    val enemies: Seq[Set[String]] = csv.allFrom(5,1).map(s => s.split(',').toSet)
    val ne: Option[String] = enemies.flatten.filter(_ != "").find(e => !names.contains(e))
    if (ne.nonEmpty) Logger.error("Non-existant enemy name : " + ne.get)

    // Get the list of guests' groups
    val groups: Seq[Set[String]] = csv.allFrom(6,1).map(s => s.split(' ').toSet)
    val ng: Option[String] = groups.flatten.filter(_ != "").find(g => !possGroups.contains(g))
    if (ng.nonEmpty) Logger.error("Non-existant group name : " + ng.get)

    val guests: Seq[Guest] = for (i <- names.indices if inclusions(i)) yield Guest(names(i), joints(i), ages(i), languages(i), enemies(i), groups(i))
    val tables: Seq[Table] = tableSizes.zipWithIndex.map(c => Table(c._2.toString, c._1, Set[Guest]()))

    (guests.toSet, tables.toSet)
  }

}
