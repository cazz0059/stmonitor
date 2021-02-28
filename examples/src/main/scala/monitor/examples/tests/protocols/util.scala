package monitor.examples.tests.protocols

object util {
  def add(x : Int, y : Int): Boolean = {
    val z = x+y
    return (z>x)&&(z>y)
  }
  def validateTok(token: String, uname: String): Boolean = {
    true
  }
}