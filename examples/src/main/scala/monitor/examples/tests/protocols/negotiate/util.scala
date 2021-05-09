//package monitor.examples.tests.protocols.negotiate

object util {
  def validate(x : Int, check : Boolean): Boolean ={
    var i = x
    while (i < 5){
      i = i + 1
    }
    (i == 4) && check
  }
}
