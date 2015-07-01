package tabler

case class Table(val size: Int,
                 val occupants: Set[Guest]) {

  def contains(g: Guest): Boolean = occupants.contains(g)

}
