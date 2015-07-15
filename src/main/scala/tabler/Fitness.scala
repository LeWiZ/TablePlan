package tabler

object Fitness {
  val jFac: Double = 0.35d
  val aFac: Double = 0.15d
  val lFac: Double = 0.05d
  val eFac: Double = 0.35d
  val gFac: Double = 0.1d

  def apply(gue: Guest, tab: Table): Double = {
    val others: Set[Guest] = tab.occupants - gue

    // Check the presence of the joint at the table, if any
    val jFit: Double = {
      if (gue.joint == "") 0.0
      else if (tab.contains(gue.joint)) 1.0d
      else 0.0d
    }

    // Age homogeneity
    val ageRat: Seq[Double] = others.toSeq.map(g => (gue.age.toDouble - g.age.toDouble).abs / gue.age.toDouble)
    val aFit: Double = (2.0d - ageRat.sum / ageRat.length) / 2.0d

    // Shared language
    val lFit: Double = others.toSeq.map(o => gue.langageProximity(o)).sum / others.size

    // Presence of enemies
    val eFit: Double = if (others.exists(o => gue.enemies.contains(o.name))) 0.0d else 1.0d

    // Shared group
    val gFit: Double = others.count(g => !g.groups.intersect(gue.groups).isEmpty).toDouble / others.size

    // Consolidate partial fitnesses with factors
    jFit * jFac + aFit * aFac + lFit * lFac + eFit * eFac + gFit * gFac

  }

  def apply(tab: Table): Double = tab.occupants.toSeq.map(this(_,tab)).sum

  def apply(scn: Scenario): Double = scn.tables.toSeq.map(this(_)).sum
}
