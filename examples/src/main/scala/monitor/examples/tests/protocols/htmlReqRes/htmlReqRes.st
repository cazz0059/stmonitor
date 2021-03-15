S_requestResponse = &{
    ?Get(s : String).+{
        !Sc200(s2 : String),
        !Sc500(reason : String)[not util.checkRequest(s)]
    }
}