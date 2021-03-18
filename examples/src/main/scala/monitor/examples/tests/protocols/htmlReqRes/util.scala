package monitor.examples.tests.protocols.htmlReqRes

object util {
  def checkString(s : String) : Boolean = {
    var s_temp = s
    while (s_temp.trim().nonEmpty){
      s_temp = s_temp.dropRight(1)
    }
    s_temp.trim().isEmpty
  }
}