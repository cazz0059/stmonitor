S_arithmetic =&{
   ?Add(x : Int, y : Int)[util.add(x, y)].+{
         !Answer1(z : Int)[z == (x+y)],
         !Invalid1(z : Int)[(z < x) || (z < y)]
     },
   ?Negate(x : Int)[!(x==0)].+{
        !Answer2(z : Int)[z == (0-x)],
        !Invalid2()[x == 0]
   },
   ?Sqrt(x : Int)[x > 0].+{
        !Answer3(y : Int)[(x >= 0) && (y == util.square_root(x))],
        !Invalid3()[x < 0]
    }
}
