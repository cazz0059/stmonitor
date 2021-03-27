S_recur = +{
    !Offer().&{
        ?Option1(x : Int)[x < 5].+{
            !Accept1()[util.repeat(x)],
            !Reject1()[!util.repeat(x)]
        },
        ?Option2(x : Int)[x >= 5].+{
            !Accept2()[util.repeat(x)],
            !Reject2()[!util.repeat(x)]
        }
    }
}