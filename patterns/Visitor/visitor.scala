/* Thanks to pattern matching you don't really need it. A visitor is really
 * just a function. Here's the wikipedia visitor example redone */

sealed abstract class CarPart
trait Composite {
  def components: Seq[CarPart]
}

object Composite {
  def unapply(x: CarPart) = {
    x match {
      case (x: Composite) => Some(x.components)
      case _ => None
    }
  }
}

case class Wheel(name: String) extends CarPart
case class Engine() extends CarPart
case class Body() extends CarPart
case class Car() extends CarPart with Composite {
  lazy val components = List(
    Wheel("front left"),
    Wheel("front right"),
    Wheel("back left"),
    Wheel("back right"),
    Body(),
    Engine())
}

object VisitorDemo {

  def printParts(part: CarPart) {
    part match {
      case Wheel(name) => println("Visiting " + name + " wheel")
      case Engine() => println("Visiting engine")
      case Body() => println("Visiting body")
      case Car() => println("Visiting car") 
    }
  }

  def useParts(part: CarPart) {
    part match {
      case Wheel(name) => println("Kicking my " + name + " wheel")
      case Engine() => println("Starting my engine")
      case Body() => println("Moving my body")
      case Car() => println("Starting my car")
    }
  }

  def recurse(f: CarPart => Unit)(x: CarPart) {
    x match {
      case Composite(components) => components.foreach(recurse(f))
      case _ => ()
    }
    f(x)
  }

  def main(args: Array[String]) {
    recurse(printParts)(Car())
    recurse(useParts)(Car())
  }
}
