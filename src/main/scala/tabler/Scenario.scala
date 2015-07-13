package tabler

case class Scenario(tables: Set[Table]) {

  /** Returns the number of free seats in the scenario */
  def free: Int = tables.toSeq.map(_.free).sum

  /** Returns all tables with free seat(s) */
  def freeTables: Set[Table] = tables.filter(_.free > 0)

  /** Returns all non-empty tables */
  def nonEmptyTables: Set[Table] = tables.filter(_.occupants.size > 0)

  /** Adds the given guest to the given table */
  def add(g: Guest, t: Table): Scenario = Scenario((tables - t) + (t.add(g)))

  /** Removes the given guest from the given table */
  def remove(g: Guest, t: Table): Scenario = Scenario((tables - t) + (t.remove(g)))

  /** Returns the number of guests seated */
  def countGuests: Int = tables.toSeq.map(_.occupants.size).sum

  override def toString: String = {
    tables.map(t => "Table " + t.id + "\n" + t.occupants.map(g => "  " + g.name + "\n" ).mkString).mkString
  }

}
