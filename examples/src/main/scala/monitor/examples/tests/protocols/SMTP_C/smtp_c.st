C_smtp = ?TwoTwoZero(option : Int)[(option == 1) || (option == 2)].rec X1.(
    &{
        ?TwoFiveZeroDash(string : String).X1,
        ?TwoFiveZero(choice : Int)[(choice == option) && (option == 1)].rec Z1.(
            !Data(data : String)[(choice == 3) || (choice == 4) || (choice == 5)].rec Z3.(
                 +{
                     !Subject(subject : String)[choice == 3].Z3,
                     !Dataline(dataline : String)[choice == 4].Z3,
                     !atad(dot : String)[(choice == 5) && (dot == 6)].Z1
                 }
             )
        )
    }
)