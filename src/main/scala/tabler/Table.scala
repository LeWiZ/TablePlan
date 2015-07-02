package tabler

case class Table(val id: String,
                 val size: Int,
                 val occupants: Set[Guest]) {

  def contains(g: Guest): Boolean = occupants.contains(g)

}
