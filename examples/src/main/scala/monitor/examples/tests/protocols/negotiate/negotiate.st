S_negotiate = +{
    !Propose(proposal : String, i : Int)[i == 0].rec START.(
        &{
            ?Accept()[i > 0].+{
                !Confirm()
            },
            ?Reject()[i < 0],
            ?Propose(proposal2 : String, i2 : Int)[i == 0].+{
                !Accept(check :Boolean)[util.validate(i, check)].&{
                    ?Confirm()
                },
                !Reject()[i2 < 0],
                !Propose(proposal3 : String, i3 : Int)[i2 >= 0].START
            }
        }
    )
}