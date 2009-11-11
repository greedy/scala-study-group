object Example {

  val convertToInt = { x:String => x.toInt }
  val doubleIt = { x:Int => x * 2 }
  val sqrtIt = { x:Double => Math.sqrt(x) }

  val pipeline = convertToInt andThen doubleIt andThen int2double andThen sqrtIt

  def main(args: Array[String]) {
    args.foreach(x => println(pipeline(x)))
  }

}
