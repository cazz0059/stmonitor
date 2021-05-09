package monitor.examples.tests.protocols.htmlReqRes

object util {
  def checkString(s : String) : Boolean = {
    var s_array : Array[Int] = Array(2, 4, 6, 8, 10)
    s_array = s_array ++ Array(s.length)
    var check = false
    for (id <- s_array){
      if(id == s.length)
        check = true
    }
    var i = 0
    if(check)
      i = 1
    else
      i = 2
    check
  }
}