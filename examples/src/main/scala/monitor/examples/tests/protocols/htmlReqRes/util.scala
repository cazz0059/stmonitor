package monitor.examples.tests.protocols.htmlReqRes

object util {
  def checkString(s : String) : Boolean = {
    var s_array : Array[Int] = Array(2, 4, 6, 8, 10)
    var s_elt = s_array(1)
    s_array = s_array ++ Array(s.length)
    var check = false
    for (id <- s_array){
      if(id == s.length)
        check = true
    }
    check
  }
}