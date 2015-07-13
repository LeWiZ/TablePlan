package tabler

class Fitness(val jFac: Double,
              val aFac: Double,
              val lFac: Double,
              val eFac: Double,
              val gFac: Double) {
  def apply(gue: Guest, tab: Table): Double = {
    val others: Set[Guest] = tab.occupants - gue

    // Check the presence of the joint at the table, if any
    val jFit: Double = {
      if (gue.joint == "") 0.0
      else if (tab.contains(gue.joint)) 1.0d
      else 0.0d
    }

    // Age homogeneity
    val ageDiff: Seq[Double] = others.toSeq.map(g => (gue.age - g.age).abs.toDouble)
    val aFit: Double = ageDiff.sum / (ageDiff.max * ageDiff.length)

    // Shared language
    val lFit: Double = others.toSeq.map(o => gue.langageProximity(o)).sum / others.size

    // Presence of enemies
    val eFit: Double = if (others.exists(o => gue.enemies.contains(o.name))) 1.0d else 0.0d

    // Shared group
    val gFit: Double = others.count(g => !g.groups.intersect(gue.groups).isEmpty).toDouble / others.size

    // Consolidate partial fitnesses with factors
    jFit * jFac + aFit * aFac + lFit * lFac + eFit * eFac + gFit * gFac
  }

  def apply(tab: Table): Double = tab.occupants.toSeq.map(this(_,tab)).sum

  def apply(scn: Scenario): Double = scn.tables.toSeq.map(this(_)).sum
}
