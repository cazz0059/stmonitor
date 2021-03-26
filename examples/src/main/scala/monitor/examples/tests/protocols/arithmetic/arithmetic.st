S_arithmetic =&{
   ?Add(x : Int, y : Int)[util.add(x, y)].+{
         !Answer(z : Int)[z == (x+y)],
         !Invalid(z : Int)[(z < x) || (z < y)]
     },
   ?Negate(x : Boolean)[!(x==0)].+{
        !Answer()

   },
   ?Sqrt(x : Int)[x > 0]
}
