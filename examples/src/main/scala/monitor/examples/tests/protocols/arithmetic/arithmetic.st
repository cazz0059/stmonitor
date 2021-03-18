S_arithmetic = &{
    ?Add(x : Int, y : Int)[util.add(x : Int, y : Int)].+{
        !Answer(z : Int)[z == x+y],
        !Invalid(z : Int)[(z < x) || (z < y)]
    },
    ?Negate(x : Bool)[!(x==0)].+{
        !Answer(z : Int)[z == -x],
        !Invalid()[x == 0]
    },
    ?Sqrt(x : Float)[x > 0].+{
        !Answer(y : Float)[(x >= 0) && (y == x.sqrt())],
        !Invalid()[x < 0]
    }
}