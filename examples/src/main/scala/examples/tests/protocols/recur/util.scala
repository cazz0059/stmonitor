package examples.tests.protocols.recur

object util {

//	def recur(x : Int): Boolean ={
//		if (x < -1){
//			recur(x+1)
//		}
//		else {
//			check(x)
//		}
//	}
//
//	def check(x : Int): Boolean ={
//		x == -1
//	}

  def noRecur(x : Int, n : Int) : Boolean = {
		if((n > 0) && (x != 0)){
			 x*n > 0 // check(x, n)
		}
		else if ((n < 0) && (x != 0)){
			x*(-n) > 0 // check(x, -n)
		}
		else {
			x >= 0 // (n == 0) || (x == 0)
		}
  }

  def check(x1 : Int, n1 : Int): Boolean ={
    x1*n1 > 0
  }

}

