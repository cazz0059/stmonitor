C_smtp = ?_220(option : String)[option == 'dash' || option == 'normal'].rec X1.(
    &{
        ?_250dash(string : String).X1,
        ?_250(choice : String)[choice == option && option == 'normal'].rec Z1.(
            !Data(data : String)[choice == 'subject' || choice == 'dataline' || choice == 'atad'].rec Z3.(
                +{
                    !Subject(subject : String)[choice == 'subject'].Z3,
                    !Dataline(dataline : String)[choice == 'dataline'].Z3,
                    !atad(dot : String)[choice == 'atad' && dot == '.'].?_250(String).Z1
                }
            )
        )
    }
)
