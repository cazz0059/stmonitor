S_negotiate = !Propose1(proposal : String, i : Int)[i == 0].rec START.(
    &{
          ?Accept1()[i > 0].!Confirm1(),
          ?Reject1()[i < 0],
          ?Propose2(proposal2 : String, i2 : Int)[i == 0].+{
                 !Accept2(check :Boolean)[util.validate(i, check)].?Confirm2(),
                 !Reject2()[i2 < 0],
                 !Propose3(proposal3 : String, i3 : Int)[i2 >= 0].START
             }
      }
    )
