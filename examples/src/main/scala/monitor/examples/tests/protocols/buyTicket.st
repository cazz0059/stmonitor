S_buy_ticket = ?Request(travel : Travel).!Quote(quote : Price).?Check(payment : Price)[payment > 10].+{
    !Approve(code : Code)[approve(code : Code) && (payment == 10)].!Ticket(ticket : String)[invoice(code : Code) && payment(payment : Price)],
    !Refuse(message : String)[refuse(message : String)]
}