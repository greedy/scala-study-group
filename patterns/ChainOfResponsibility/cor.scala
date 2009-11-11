object Requests {
  type Handler = PartialFunction[Request, Unit]
}

case class Request(value: Int)

object Example {

  import Requests.Handler

  val handleEvens: Handler = { case Request(x) if x % 2 == 0 => println("Handling even value " + x) }
  val handleOdds: Handler = { case Request(x) if x % 2 == 1 => println("Handling odd value " + x) }
  val handlePalindrome: Handler = { case Request(x) if x.toString.reverse.toString == x.toString => println("Handling palindrome " + x) }

  val handlerChain = handlePalindrome orElse handleEvens orElse handleOdds

  def main(args: Array[String]) {
    args.foreach { arg => handlerChain(Request(arg.toInt)) }
  }
}
