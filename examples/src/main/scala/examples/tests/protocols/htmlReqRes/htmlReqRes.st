S_requestResponse =
    ?Get(s : String).+{
        !Sc200(s2 : String),
        !Sc500(reason : String)[!util.checkString(s)]
    }