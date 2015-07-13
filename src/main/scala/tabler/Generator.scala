package tabler

import scala.util.Random

class Generator {

  def randomFill(scn: Scenario, guests: Set[Guest], cj: Boolean): Scenario = {
    if (scn.free < guests.size) Logger.error("Not enough seats for all guests")

    if (guests.isEmpty) scn
    else {
      val rguest: Guest = guests.toSeq(Random.nextInt(guests.size))
      val freetables: Set[Table] = scn.freeTables
      val rtable: Table = freetables.toSeq(Random.nextInt(freetables.size))

      val newscn: Scenario = scn.add(rguest, rtable)

      randomFill(newscn, guests - rguest, cj)
    }
  }

  def mutation(scn: Scenario, cj: Boolean): Scenario = {
    val netables: Set[Table] = scn.nonEmptyTables
    val otable: Table = netables.toSeq(Random.nextInt(netables.size))
    val g: Guest = otable.occupants.toSeq(Random.nextInt(otable.occupants.size))

    val freetables: Set[Table] = scn.freeTables
    val ntable: Table = freetables.toSeq(Random.nextInt(freetables.size))

    if (otable != ntable) scn.remove(g, otable).add(g, ntable)
    else scn
  }

  def crossOver(scn1: Scenario, scn2: Scenario, cj: Boolean): Scenario = ???

}
