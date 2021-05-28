package examples.tests.protocols.recur

object util {

//  def noRecur1(x : Int, n : Int) : Boolean = {
//    if((n > 0) && (x != 0)){
//      check(x, n)
//    }
//    else if ((n < 0) && (x != 0)){
//      check(x, -n)
//    }
//    else {
//      x >= 0// (n == 0) || (x == 0)
//    }
//  }
  
  def noRecur(x : Int, n : Int) : Boolean = {
		if((n > 0) && (x != 0)){
			 check(x, n) // x*n > 0
		}
		else if ((n < 0) && (x != 0)){
			x*(-n) > 0 // check(x, -n)
		}
		else {
			x >= 0 // (n == 0) || (x == 0)
		}
//  	if(x >= 0){
//			true // x >= 0
//  	}
//  	else {
//			false
//  	}
  }

  def check(x1 : Int, n1 : Int): Boolean ={
    x1*n1 > 0
  }

//  def check2(x2 : Int): Boolean ={
//    x2 != 5
//  }
}

// very dumb test
// the goal is to unfold the loops and functions correctly
// and find the redundancy when compared to the conditions in the corresponding branch trace

// recursion wont work because the functions have the same names
