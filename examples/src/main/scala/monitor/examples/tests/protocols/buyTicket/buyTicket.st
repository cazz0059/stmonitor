S_buy_ticket = &{
    ?Request(travel : Travel).+{
        !Quote(quote : Price).&{
            ?Check(payment : Price)[payment > 10].+{
                !Approve(code : Code)[util.approve(code) and (payment == 10)].+{
                    !Ticket(ticket : String)[util.invoice(code) and util.pay(payment)]
                },
                !Refuse(message : String)[util.refuse(message)]
            }
        }
    }
}