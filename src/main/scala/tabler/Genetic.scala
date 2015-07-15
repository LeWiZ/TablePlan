package tabler

import scala.util.Random
import scala.collection.SortedSet

class Genetic(val gen: Generator, val mRate: Double, val maxsize: Int, val cj: Boolean) {

  def randomPool(empty: Scenario, guests: Set[Guest]): Pool = {
    val scnList: Iterable[Scenario] = for (i <- 0 to maxsize) yield gen.randomFill(empty, guests, cj)
    Pool(scnList.toIndexedSeq).sorted
  }

  def killToSize(p: Pool): Pool = {
    def generateFilter(input: IndexedSeq[Boolean], n: Int): IndexedSeq[Boolean] = {
      if (n==0) input
      else {
        val rnd: Double = Random.nextDouble()
        val index: Int = (rnd * rnd * (input.size-1)).toInt
        if (input(index)) generateFilter(input.updated(index, false), n-1)
        else generateFilter(input, n)
      }
    }

    Pool(p.scenarios.zip(generateFilter(IndexedSeq.fill(p.size)(true), maxsize)).filter(_._2).unzip._1)
  }

  def step(p: Pool): Pool = {
    val newScn: Scenario = {
      if (Random.nextDouble() < mRate) gen.mutation(p.pickRandom, cj)
      else gen.crossOver(p.pickRandom, p.pickRandom, cj)
    }
    val grownPool: Pool = p.addNoSort(newScn)
    if (grownPool.size > maxsize * 2) killToSize(grownPool.sorted)
    else grownPool
  }

  def nStep(p: Pool, n: Int): Pool = if (n<=0) p else {
    val newP: Pool = step(p)
    nStep(newP, n-1)
  }

}
