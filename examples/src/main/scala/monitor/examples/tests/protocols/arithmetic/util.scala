//package monitor.examples.tests.protocols.arithmetic
import scala.math._

object util {
  def add(x : Int, y : Int): Boolean = {
    val z = x+y
    (z>=x)&&(z>=y)
  }
  def divide(x : Int, y : Int) : Int = {
    x/y
  }
}