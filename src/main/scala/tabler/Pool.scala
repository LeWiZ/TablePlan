package tabler

import scala.util.Random
import scala.collection.SortedSet

case class Pool(val scenarios: IndexedSeq[Scenario]) {

  def size: Int = scenarios.size

  def remove(n: Int): Pool = Pool(scenarios.take(n) ++ scenarios.drop(n+1))
  def removeWorst: Pool = Pool(scenarios.tail)

  def addNoSort(scn: Scenario): Pool = Pool(scenarios :+ scn)
  def sorted: Pool = Pool(scenarios.sorted(ScenarioOrdering))

  def best: Scenario = scenarios.last

  def averageFit: Double = scenarios.map(_.fitness).sum / scenarios.size

  def pickRandom: Scenario = scenarios(Random.nextInt(size))

}
