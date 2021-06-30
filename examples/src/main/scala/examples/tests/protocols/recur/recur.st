S_recur =
    !Offer().&{
        ?Option1(x : Int)[x >= 0].+{
            !Accept1(n : Int)[util.noRecur(x, n)],
            !Reject1(n : Int)[!util.noRecur(x, n)]
        },
        ?Option2(x : Int)[x < 0].+{
            !Accept2(n : Int)[util.noRecur(x, n)],
            !Reject2(n : Int)[!util.noRecur(x, n)]
        }
    }
