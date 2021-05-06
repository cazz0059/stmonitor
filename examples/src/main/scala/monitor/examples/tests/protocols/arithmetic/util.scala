package monitor.examples.tests.protocols.arithmetic

object util {
  def add(x : Int, y : Int): Boolean = {
    val z = x+y
    (z>=x)&&(z>=y)
  }
  def divide(x : Int, y : Int, z : Int) : Boolean = {
    z == x/y
  }
}