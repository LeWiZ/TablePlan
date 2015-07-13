package tabler

object Main extends App {

  if (args(0) != "") {
    println("Wedding table plan solver")
    println("-------------------------")
    println("")

    Loader("https://docs.google.com/spreadsheets/d/" + args(0) + "/export?gid=0&format=csv")
  }
  else {
    println("Usage : tabler spreadsheet_id")
    println("  spreadsheet_id : the unique token in Google Spreadsheet URL (sharing being activated)")
  }

}
