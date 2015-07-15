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

  private def move(scn: Scenario, cj: Boolean): Scenario = {
    val netables: Set[Table] = scn.nonEmptyTables
    val otable: Table = netables.toSeq(Random.nextInt(netables.size))
    val g: Guest = otable.occupants.toSeq(Random.nextInt(otable.occupants.size))

    val freetables: Set[Table] = scn.freeTables
    val ntable: Table = freetables.toSeq(Random.nextInt(freetables.size))

    if (otable != ntable) scn.remove(g, otable).add(g, ntable) else scn
  }

  private def switch(scn: Scenario, cj: Boolean): Scenario = {
    val netables: Set[Table] = scn.nonEmptyTables
    val table1: Table = netables.toSeq(Random.nextInt(netables.size))
    val table2: Table = netables.toSeq(Random.nextInt(netables.size))
    val g1: Guest = table1.occupants.toSeq(Random.nextInt(table1.occupants.size))
    val g2: Guest = table2.occupants.toSeq(Random.nextInt(table2.occupants.size))

    if (table1 != table2 && g1 != g2) {
      scn.replace(g1, g2, table1).replace(g2, g1, table2)
    } else scn
  }

  def mutation(scn: Scenario, cj: Boolean): Scenario = {
    if (Random.nextBoolean) switch(scn, cj) else move(scn, cj)
  }

  def crossOver(scn1: Scenario, scn2: Scenario, cj: Boolean): Scenario = ???

}
