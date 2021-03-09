package monitor.examples.tests.protocols.recur

object util {
  def repeat(x : Int): Boolean ={
    if (x < 5){
      repeat(x+1)
    }
    else {
      check(x)
    }
  }
  def check(x : Int): Boolean ={
    x == 5
  }
}

// very dumb test
// the goal is to unfold the loops and functions correctly
// and find the redundancy when compared to the conditions in the corresponding branch trace
