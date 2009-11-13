object WithTraits {
  trait Storage {
    protected def read(key: String): String
    protected def write(key: String, value: String): Unit
  }

  trait Persistence { this: Storage =>
    def save(key: String, value: String): Unit = write(key, value)
    def load(key: String): String = read(key)
  }

  trait FileStorage extends Storage {
    def read(key: String): String = {
      println("Reading " + key + " from files")
      "foo"
    }
    def write(key: String, value: String): Unit = {
      println("Writing " + value + " for " + key + " from files")
    }
  }

  trait DatabaseStorage extends Storage {
    protected def read(key: String): String = {
      println("Reading " + key + " from database")
      "bar"
    }
    protected def write(key: String, value: String): Unit = {
      println("Writing " + value + " for " + key + " from database")
    }
  }

  trait CacheDriver {
    protected def get(key: String): Option[String]
    protected def put(key: String, value: String): Unit
  }

  trait CachedPersistence extends Persistence { this: Storage with CacheDriver =>
    override def save(key: String, value: String): Unit = {
      put(key, value)
      super.save(key, value)
    }
    override def load(key: String): String = {
      get(key) match {
        case Some(value) => value
        case None => {
          val value = super.load(key)
          put(key, value)
          value
        }
      }
    }
  }

  trait SingleCache extends CacheDriver {
    var cachedKey: String = _
    var cachedValue: String = _
    protected def put(key: String, value: String): Unit = {
      cachedKey = key
      cachedValue = value
    }
    protected def get(key: String): Option[String] = {
      if (key == cachedKey) Some(cachedValue) else None
    }
  }

  def main(args: Array[String]) {
    val disk = new Persistence with FileStorage
    val db = new Persistence with DatabaseStorage
    val cachedDisk = new CachedPersistence with FileStorage with SingleCache

    println("""
      |Created three difference persistence things:
      |disk: Persistence with FileStorage
      |db: Persistence with DatabaseStorage
      |cachedDisk: CachedPersistence with FileStorage with SingleCache

      |For each in turn, perform two operations:
      |1. save("foo", "bar")
      |2. load("foo")
      """.stripMargin)

    println("disk")
    disk.save("foo", "bar")
    disk.load("foo")

    println("database")
    db.save("foo", "bar")
    db.load("foo")

    println("cached disk")
    cachedDisk.save("foo", "bar")
    cachedDisk.load("foo")
  }
}
