package examples.tests.protocols.listRes

object util {
  def checkPrivilege(subject:String, resource:String, operation:String, certificate:String): Boolean = {
    val flag1 = subject.contains("http") || subject.contains("https")
    val flag2 = resource.contains("http") || resource.contains("https")
    val flag3 = operation.trim().nonEmpty
    val flag4 = certificate.trim().nonEmpty
    ((flag1 && flag2) && flag3) && flag4
  }
}