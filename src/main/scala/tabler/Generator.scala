package tabler

trait Generator {
  def randomFill(empty: Scenario, guests: Set[Guest], cj: Boolean): Scenario
  def mutation(scn: Scenario, cj: Boolean): Scenario
  def crossOver(scn1: Scenario, scn2: Scenario, cj: Boolean): Scenario
}
