
import Math.max
import Vector._

/**
 * <p>An immutable implementation of the {@link Seq} interface with an array
 * backend.  Effectively, this class is an immutable vector.  The only wrinkle
 * in this design is the ability to store elements in <i>completely</i> arbitrary
 * indexes (to enable use as a random-access array).  Thus, the length of this
 * data structure is effectively infinite.  The {@link #length} field is defined
 * to return the maximum index of the elements in the vector.</p>
 * 
 * <p>The underlying data structure for the persistent vector is a trie with an
 * extremely high branching factor (in this case: 32).  Each trie node contains
 * an array representing each branch.  This implementation allows almost-constant
 * time access coupled with minimal data-copying on insert.  The ratio between
 * these two is controlled by the branching factor.  A higher branching factor
 * will lead to a better worst-case access time (asymtotically more constant)
 * while a lower branching factor will result in asymtotically less data
 * copying on insert.</p>
 *
 * <p>As is natural for an immutable data structure, this vector is parameterized
 * covariantly.  This poses a few minor difficulties in implementation, due to
 * the fact that arrays, as mutable containers, are parameterized
 * <i>invariantly</i>.  To get around this, some up-casting is utilized durring
 * insertion.  This is considered to be sound due to the fact that the type
 * system will ensure that the casting is always upwards, rather than down (which
 * is where the mutability concerns come into play).</p>
 * 
 * @author Daniel Spiewak
 */
class Vector[+T] private (private val data: Option[T], val length: Int, val branchingFactor: Int,
          private val left: Seq[Vector[T]], private val right: Seq[Vector[T]]) extends RandomAccessSeq[T] {
  
  def this(branchingFactor: Int) = this(None, 0, branchingFactor, EmptyArray, EmptyArray)
  
  def this() = this(32)
  
  def apply(i: Int) = locate(computePath(i, branchingFactor))
  
  private def locate(path: List[Int]): T = path match {
    case hd :: tail => {
      val branches = if (hd < (branchingFactor / 2)) left else right
      val node = if (hd < branches.length) branches(hd) else null.asInstanceOf[Vector[T]]
      
      if (node == null) null.asInstanceOf[T] else node.locate(tail)
    }
    
    case Nil => data.get
  }
  
  def +[A >: T](e: A) = update(length, e)
  
  def update[A >: T](i: Int, e: A) = store(computePath(i, branchingFactor), e)
  
  private def store[A >: T](path: List[Int], e: A): Vector[A] = path match {
    case hd :: tail => {
      val branches = if (hd < (branchingFactor / 2)) left else right
      val node = if (hd < branches.length) branches(hd) else null.asInstanceOf[Vector[T]]
      val vector = if (node == null) EmptyVector(branchingFactor) else node
      
      val newBranches = new Array[Vector[A]](max(branches.length, hd + 1))
      Array.copy(branches, 0, newBranches, 0, branches.length)
      
      newBranches(hd) = vector.store(tail, e)
      
      val newLeft = if (hd < (branchingFactor / 2)) newBranches else left
      val newRight = if (hd < (branchingFactor / 2)) right else newBranches
      
      new Vector(data, max(length, flattenPath(path, branchingFactor) + 1), branchingFactor, newLeft, newRight)
    }
    
    case Nil => new Vector(Some(e), max(length, flattenPath(path, branchingFactor) + 1), branchingFactor, left, right)
  }
  
  override def map[A](f: (T)=>A): Vector[A] = {
    def mapFun(vec: Vector[T]) = if (vec == null) null else vec map f
    
    val newLeft = left map mapFun
    val newRight = right map mapFun
    
    new Vector(data map f, length, branchingFactor, newLeft, newRight)
  }
  
  override def equals(other: Any) = other match {
    case vec:Vector[T] => {
      var back = length == vec.length
      for (i <- 0 until length) {
        back &&= this(i) == vec(i)
      }
      
      back
    }
    
    case _ => false
  }
}

object Vector {
  val EmptyArray = new Array[Vector[Nothing]](0)
  
  private var pathCache = Map[(Int, Int), List[Int]]()
  
  def apply[T](elems: T*) = {
    var vector = new Vector[T]
    var i = 0
    
    for (e <- elems) {
      vector = vector(i) = e
      i += 1
    }
    vector
  }
  
  def computePath(total: Int, base: Int): List[Int] = {
    if (total < 0) {
      throw new IndexOutOfBoundsException(total.toString)
    } else if (total < base) List(total) else {
      if (pathCache contains (total, base)) pathCache((total, base)) else {
        val back = {
          var num = total
          var digits = 0
          while (num >= base) {
            num /= base
            digits += 1
          }
          
          val rem = total % (Math.pow(base, digits)).toInt
          
          val subPath = computePath(rem, base)
          num :: (0 until (digits - subPath.length)).foldRight(subPath) { (i, path) => 0 :: path }
        }
        pathCache += ((total, base) -> back)
        
        back
      }
    }
  }
  
  @inline
  def flattenPath(path: List[Int], base: Int) = path.foldLeft(0) { _ * base + _ }
}

object EmptyVector {
  private var cache = Map[Int, Vector[Nothing]]()
  
  def apply(branchingFactor: Int) = {
    if (cache contains branchingFactor) cache(branchingFactor) else {
      val back = new Vector[Nothing](branchingFactor)
      cache += (branchingFactor -> back)
      
      back
    }
  }
}
