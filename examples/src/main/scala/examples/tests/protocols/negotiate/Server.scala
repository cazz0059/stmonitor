package examples.tests.protocols.negotiate

import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext
import scala.util.Random

import lchannels.In

object Server {
  def apply(Client: In[Propose1])(implicit ec: ExecutionContext, timeout: Duration) {

    var negotiateChn = Client
    println("[S] Server started, to terminate press CTRL+c")

    negotiateChn ? {
      case msg@Propose1(_, _) =>
        println(f"[S] Receiving First Proposal(${msg.proposal}, ${msg.i})")
        var start = true

        while (start) {
          val proposal2 = Random.alphanumeric.filter(_.isLetter).take(20).mkString
          val i2 = -5
          println(f"[S] Sending Alternative Proposal($proposal2, $i2)")
          var reqChn = msg.cont !! Propose2(proposal2, i2) _
          reqChn ? {
//            case msg@Accept2(_) =>
//              println(f"[S] Receiving Acceptance()")
//              println(f"$proposal accepted with parameter ${msg.check}")
//              start = false
            case msg@Reject2() =>
              println(f"[S] Receiving Rejection()")
              println(f"$proposal2 rejected")
              start = false
            case msg@Propose3(_, _) =>
              println(f"[S] Receiving Second Proposal(${msg.proposal3}, ${msg.i3})")
          }
        }
    }
  }
}

//var negotiateChn = Client
//println("[S] Server started, to terminate press CTRL+c")
//authChn ! {
//  case msg @ Propose1(_, _) =>
//    println(f"[S] Sending First Proposal(${msg.proposal}, ${msg.i})")
//    val token = Random.alphanumeric.filter(_.isLetter).take(10).mkString
//    println(f"[S] Receiving Succ($token)")
//    var reqChn = msg.cont !! Succ(token)_
//    notAuth = false
//    while(!notAuth){
//      reqChn ? {
//        case msg @ Get(_, _) =>
//          println(f"[S] Received Get(${msg.resource}, ${msg.reqTok})")
//          if(token!=msg.reqTok){
//            println("[S] Tokens do not match: sending Timeout()")
//            authChn = msg.cont !! Timeout()
//            notAuth = true
//          } else {
//            println(f"[S] Sending Res(content)")
//            reqChn = msg.cont !! Res("content")
//          }
//        case msg @ Rvk(_) =>
//          println(f"[S] Received Rvk(${msg.rvkTok}): terminating")
//          return
//      }
//    }
//}