package monitor.examples.tests.protocols.arithmetic
import scala.math._

object util {
  def add(x : Int, y : Int): Boolean = {
    val z = x+y
    (z>=x)&&(z>=y)
  }
  def square_root(x : Int) : Int = {
    val x1 = x.toFloat
    val x1_sqrt = sqrt(x1)
    x1_sqrt.toInt
  }
}