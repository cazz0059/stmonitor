S_listRes = ?Request(resource_kind : String, subject:String, resource:String, operation:String, certificate:String)[util.checkPrivilege(subject, resource, operation, certificate)].+{
    !Ok().rec X.(
                 +{
                     !Response(element : String).X,
                     !Completed()
                 }
             ),
    !Error(reason:String)[(operation == null) || (certificate == null)]
}