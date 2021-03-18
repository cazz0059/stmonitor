S_recur = +{
    !Offer().&{
        ?Option1(x : Int)[x < 5].+{
            !Accept()[util.repeat(x)],
            !Reject()[!util.repeat(x)]
        },
        ?Option2(x : Int)[x >= 5].+{
            !Accept()[util.repeat(x)],
            !Reject()[!util.repeat(x)]
        }
    }
}