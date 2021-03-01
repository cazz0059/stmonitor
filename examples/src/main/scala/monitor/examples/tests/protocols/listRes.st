S_listRes = ?Request(resource_kind : String)[!(resource_kind == null)].rec X.(
    +{
        !Invalid()[resource_kind == null],
        !Response(element : Format).X,
        !Completed()
    }
)