package tabler

case class Table(val id: String,
                 val size: Int,
                 val occupants: Set[Guest]) {

  /** Returns whether a guest is seated at the table */
  def contains(g: Guest): Boolean = occupants.contains(g)

  /** Returns whether a guest is seated at the table */
  def contains(g: String): Boolean = occupants.exists(_.name == g)

  /** Returns the number of free seats at the table */
  def free: Int = size - occupants.size

  /** Adds the given guest at the table */
  def add(g: Guest): Table = Table(id, size, occupants + g)

  /** Removes the given guest from the table */
  def remove(g: Guest): Table = Table(id, size, occupants - g)

}
