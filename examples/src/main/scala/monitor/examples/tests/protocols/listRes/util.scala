package monitor.examples.tests.protocols.listRes

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
}