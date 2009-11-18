import java.lang.Integer

trait Toolkit {
  def drawLine(x0: Int, y0: Int, x1: Int, y1: Int): Unit
}

object Java2DToolkit extends Toolkit {
  def drawLine(x0: Int, y0: Int, x1: Int, y1: Int) {
    format("Drawing Java2D line from (%d,%d) to (%d,%d)\n", x0:Integer, y0:Integer, x1:Integer, y1:Integer)
  }
}

object SWTToolkit extends Toolkit {
  def drawLine(x0: Int, y0: Int, x1: Int, y1: Int) {
    format("Drawing SWT line from (%d,%d) to (%d,%d)\n", x0:Integer, y0:Integer, x1:Integer, y1:Integer)
  }
}

object WithMethodImplicits {

  trait Shape {
    def draw(implicit tk: Toolkit): Unit
  }

  case class Square(size: Int) extends Shape {
    def draw(implicit tk: Toolkit) {
      import tk._
      drawLine(0,0,0,size)
      drawLine(0,size,size,size)
      drawLine(size,size,size,0)
      drawLine(size,0,0,0)
    }
  }

  case class RightTriangle(a: Int, b: Int) extends Shape {
    def draw(implicit tk: Toolkit) {
      import tk._
      drawLine(0,0,0,a)
      drawLine(0,a,b,0)
      drawLine(b,0,0,0)
    }
  }

  def main(args: Array[String]) {
    j2d()
    swt()
  }

  def j2d() {
    implicit val toolkit = Java2DToolkit
    Square(5).draw
    RightTriangle(3,4).draw
  }

  def swt() {
    implicit val toolkit = SWTToolkit
    Square(5).draw
    RightTriangle(3,4).draw
  }
}

object WithClassImplicits {

  trait Shape {
    def tk: Toolkit
    def draw(): Unit
  }

  case class Square(size: Int)(implicit val tk: Toolkit) extends Shape {
    def draw() {
      import tk._
      drawLine(0,0,0,size)
      drawLine(0,size,size,size)
      drawLine(size,size,size,0)
      drawLine(size,0,0,0)
    }
  }

  case class RightTriangle(a: Int, b: Int)(implicit val tk: Toolkit) extends Shape {
    def draw() {
      import tk._
      drawLine(0,0,0,a)
      drawLine(0,a,b,0)
      drawLine(b,0,0,0)
    }
  }

  def main(args: Array[String]) {
    j2d()
    swt()
  }

  def j2d() {
    implicit val toolkit = Java2DToolkit
    Square(5).draw
    RightTriangle(3,4).draw
  }

  def swt() {
    implicit val toolkit = SWTToolkit
    Square(5).draw
    RightTriangle(3,4).draw
  }
}
