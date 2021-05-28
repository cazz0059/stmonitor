S_buy_ticket = ?Request(travel: String).!Quote(quote: Int).?Check(payment: Int)[payment > 10].+{
            !Approve(code: String)[util.approve(code) && (payment==10)].!Ticket(ticket: String)[util.invoice(code) && util.pay(payment)],
            !Refuse(message: String)[util.refuse(message)]
        }