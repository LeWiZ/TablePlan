package tabler

object Main extends App {

  if (args(0) != "") {
    println("")
    println("Wedding table plan solver")
    println("-------------------------")
    println("")

    Logger(1, "Loading Google Spreadsheet...")

    val data: (Set[Guest], Set[Table]) = Loader("https://docs.google.com/spreadsheets/d/" + args(0) + "/export?gid=0&format=csv")
    val guests: Set[Guest] = data._1
    val empty: Scenario = Scenario(data._2)

    val generator: Generator = new Generator
    val genetic: Genetic = new Genetic(generator, 1.0d, 100, false)

    Logger(1, "Initializing pool with random tables...")
    val initPool: Pool = genetic.randomPool(empty, guests)

    def iterations(p: Pool, i: Int, m: Int, step: Int): Pool = {
      if (i >= m) p
      else {
        val np: Pool = genetic.nStep(p, step)
        Logger(2, (i+step).toString + " iterations done. Best fitness is " + np.best.fitness + ".")
        iterations(np, i+step, m, step)
      }
    }

    Logger(1, "Genetic iterations...")
    val newPool: Pool = iterations(initPool, 0, 1000000, 10000)

    println("Pool size : " + newPool.size.toString)
    println("Best fitness : " + newPool.best.fitness.toString)
    println("Total guests : " + newPool.best.tables.toSeq.map(_.occupants.size).sum.toString)
    println("Total tables : " + newPool.best.tables.size.toString)
    println("")
    println(newPool.best)
  }
  else {
    println("Usage : tabler spreadsheet_id")
    println("  spreadsheet_id : the unique token in Google Spreadsheet URL (sharing being activated)")
  }

  println("")

}
