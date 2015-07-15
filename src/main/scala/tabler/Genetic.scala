package tabler

import scala.util.Random
import scala.collection.SortedSet

class Genetic(val gen: Generator, val mRate: Double, val cj: Boolean) {

  def randomPool(size: Int, empty: Scenario, guests: Set[Guest]): Pool = {
    val scnList: Iterable[Scenario] = for (i <- 0 to size) yield gen.randomFill(empty, guests, cj)
    Pool(SortedSet[Scenario]()(ScenarioOrdering) ++  scnList)
  }

  def killOne(p: Pool): Pool = {
    val rnd: Double = Random.nextDouble()
    val index: Int = (rnd * rnd * rnd * (p.size-1)).toInt
    p.remove(index)
    // p.removeWorst
  }

  def step(p: Pool): Pool = {
    val newScn: Scenario = {
      if (Random.nextDouble() < mRate) gen.mutation(p.pickRandom, cj)
      else gen.crossOver(p.pickRandom, p.pickRandom, cj)
    }
    val grownPool: Pool = p.add(newScn, Fitness(newScn))
    if (grownPool.size > p.size) killOne(grownPool)
    else grownPool
  }

  def nStep(p: Pool, n: Int): Pool = if (n<=0) p else {
    val newP: Pool = step(p)
    nStep(newP, n-1)
  }

}
