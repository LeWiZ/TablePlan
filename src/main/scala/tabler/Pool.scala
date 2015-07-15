package tabler

import scala.util.Random
import scala.collection.SortedSet

case class Pool(val scenarios: SortedSet[Scenario]) {

  def size: Int = scenarios.size

  def remove(n: Int): Pool = Pool(scenarios.take(n) ++ scenarios.drop(n+1))
  def removeWorst: Pool = Pool(scenarios.tail)

  def add(scn: Scenario, fit: Double): Pool = Pool(scenarios + scn)

  def best: Scenario = scenarios.last

  def averageFit: Double = scenarios.map(_.fitness).sum / scenarios.size

  def pickRandom: Scenario = scenarios.toIndexedSeq(Random.nextInt(size))

}
