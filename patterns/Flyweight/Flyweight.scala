import java.lang.ref.WeakReference

import scala.collection._

trait FlyweightCompanion {
  type Key
  type T

  def create(k: Key): T

  val values = new mutable.HashMap[Key,WeakReference[T]]

  def apply(k: Key): T = {
    def add(k: Key): T = {
      val v = create(k)
      if (v != null)
        values.update(k, new WeakReference(v))
      v
    }
    values.get(k) match {
      case Some(ref) =>
        val v = ref.get
        if (v == null)
          add(k)
        else
          v
      case None => add(k)
    }
  }

  def groom() = {
    values.retain{case (k,v) => v.get != null}
  }
}
