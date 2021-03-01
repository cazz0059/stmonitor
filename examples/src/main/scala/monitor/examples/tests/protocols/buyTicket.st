S_buy_ticket = ?Request(travel : Travel).!Quote(quote : Price).?Check(payment : Price)[payment > 10].+{
    !Approve(code : Code)[approve(code) && (payment == 10)].!Ticket(ticket : String)[invoice(code) && payment(payment)],
    !Refuse(message : String)[refuse(message)]
}