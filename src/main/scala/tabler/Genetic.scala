package tabler

import scala.util.Random

class Genetic(val fit: Fitness, val gen: Generator, val mRate: Double, val cj: Boolean) {

  def randomPool(size: Int, empty: Scenario, guests: Set[Guest]): Pool = {
    val scnList: Iterable[Scenario] = for (i <- 0 to size) yield gen.randomFill(empty, guests, cj)
    val fitList: Iterable[(Scenario, Double)] = scnList map (s => (s, fit(s)))
    Pool(fitList.toIndexedSeq.sortBy(_._2))
  }

  def killOne(p: Pool): Pool = {
    val rnd: Double = Random.nextDouble()
    val index: Int = (rnd * rnd * p.size).toInt
    p.remove(index)
  }

  def step(p: Pool): Pool = {
    val newScn: Scenario = {
      if (Random.nextDouble() < mRate) gen.mutation(p.pickRandom, cj)
      else gen.crossOver(p.pickRandom, p.pickRandom, cj)
    }
    val grownPool: Pool = p.add(newScn, fit(newScn))
    if (grownPool.size > p.size) killOne(grownPool)
    else grownPool
  }

  def nStep(p: Pool, n: Int): Pool = if (n<=0) p else {
    val newP: Pool = step(p)
    nStep(newP, n-1)
  }

}
