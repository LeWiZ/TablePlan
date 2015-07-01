package tabler

case class Language(val str: String)
case class Group(val str: String)

case class Guest(val name: String,
                 val joint: Option[Guest],
                 val age: Int,
                 val languages: Set[Language],
                 val enemies: Set[Guest],
                 val groups: Set[Group])
