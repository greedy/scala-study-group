object Font extends FlyweightCompanion {
  type Key = String
  type T = Font

  def create(name: String) = {
    println("Loading font " + name)
    new Font(name) with NotNull
  }
}

class Font private (name: String) {
  override def toString = "Font name="+name
}
