package monitor.examples.tests.protocols.buyTicket

object util {
  def add(x : Int, y : Int): Boolean = {
    val z = x+y
    return (z>=x)&&(z>=y)
  }
  def checkPrivilege(subject:String, resource:String, operation:String, certificate:String): Boolean = {
    val flag1 = subject.contains("http") || subject.contains("https")
    val flag2 = resource.contains("http") || resource.contains("https")
    val flag3 = !operation.trim().isEmpty()
    val flag4 = !certificate.trim().isEmpty()
    return ((flag1 && flag2) && flag3) && flag4
  }
  def checkString(s : String) : Boolean = {
    var s_temp = s
    while (!s_temp.trim().isEmpty()){
      s_temp = s_temp.dropRight(1)
    }
    return s_temp.trim().isEmpty()

  }
  def approve(code : String): Boolean ={
    true
  }
  def invoice(code : String): Boolean ={
    true
  }
  def pay(payment : Float): Boolean ={
    true
  }
  def refuse(message : String): Boolean ={
    true
  }
}