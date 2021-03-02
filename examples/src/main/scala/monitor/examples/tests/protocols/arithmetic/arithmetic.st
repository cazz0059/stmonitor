S_arithmetic = &{
    ?Add(x : Int, y : Int)[util.add(x : Int, y : Int)].!Answer(z : Int)[z == x+y],
    ?Negate(x : Bool)[!(x==0)].!Answer(z : Int)[z == -x],
    ?Sqrt(x : Float)[x > 0].+{
        !Answer(y : Float)[(x >= 0) && (y == x.sqrt())],
        !Invalid()[x < 0]
    }
}