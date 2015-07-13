package tabler

case class Table(val id: String,
                 val size: Int,
                 val occupants: Set[Guest]) {

  def contains(g: Guest): Boolean = occupants.contains(g)
  def contains(g: String): Boolean = occupants.exists(_.name == g)

}
