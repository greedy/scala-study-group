/* Scala makes it easy to use persistent immutable objects. This kind of
 * obviates the need for memento. Program in this style and objects are their
 * own mementos. */

class Document private (lines: Vector[String]) {
  def this() = this(Vector())

  def appendLine(line: String) = new Document(lines + line)

  def updateLine(n: Int, line: String) = new Document(lines(n) = line)

  def content = lines.mkString("\n")
}


object Memento {
  def main(args: Array[String]) {
    var versions: List[Document] = Nil

    def edit(f: Document => Document) {
      versions = versions match {
        case (d::ds) => f(d)::versions
        case Nil => List(f(new Document))
      }
    }

    def undo() {
      versions = versions match {
        case d::ds => ds
        case Nil => Nil
      }
    }

    def show() {
      println("--- Start ---")
      versions match {
        case d::_ => println(d.content)
        case _ => ()
      }
      println("--- End ---")
    }

    edit(_.appendLine("foo"))
    edit(_.appendLine("bat"))
    edit(_.appendLine("baz"))
    edit(_.appendLine("quux"))
    show()
    undo()
    edit(_.updateLine(1, "bar"))
    show()
    undo()
    undo()
    show()
  }
}
