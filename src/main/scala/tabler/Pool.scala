package tabler

import scala.util.Random

case class Pool(val scenarios: IndexedSeq[(Scenario, Double)]) {

  def size: Int = scenarios.size

  def remove(n: Int): Pool = Pool(scenarios.take(n) ++ scenarios.drop(n+1))

  def add(scn: Scenario, fit: Double): Pool = Pool((scenarios :+ ((scn, fit))).sortBy(_._2))

  def best: Scenario = scenarios.last._1

  def pickRandom: Scenario = scenarios(Random.nextInt(size))._1

}
