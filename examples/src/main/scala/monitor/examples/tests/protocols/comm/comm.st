S_comm = +{
    !M1(x : int)[x > 5].rec X.(
        &{
            ?M2(msg : String)[util.check(x)],
            ?M3(msg : String).X
        }
    )
}