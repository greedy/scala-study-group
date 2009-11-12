object SomeStreams {
  def randomInt(seed: Long): (Int, Long) = {
    val nextSeed = (seed * 0x5deece66dL + 0xbL) & ((1L << 48) - 1)
    val randVal = (seed >>> 16).toInt
    (randVal, nextSeed)
  }

  def randomInts: Stream[Int] = randomInts(new java.util.Random().nextLong)

  def randomInts(seed: Long): Stream[Int] = {
    val (randVal, nextSeed) = randomInt(seed)
    Stream.cons(randVal, randomInts(nextSeed))
  }

  def fibs: Stream[Int] = {
    Stream.cons(0, Stream.cons(1, fibs.zip(fibs.drop(1)).map{case (a,b) => a + b}))
  }

  /* algorithm from http://www.comlab.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf */
  def pidigits: Stream[Int] = {
    def g(q: BigInt, r: BigInt, t: BigInt, k: BigInt, n: BigInt, l: BigInt): Stream[Int] = {
      if (q*4+r-t<n*t) {
        Stream.cons(n.intValue, g(q*10,(r-n*t)*10,t,k,((q*3+r)*10)/t-n*10,l))
      } else {
        g(q*k,(q*2+r)*l,t*l,k+1,(q*(k*7+2)+r*l)/(t*l),l+2)
      }
    }
    g(1,0,1,1,3,3)
  }

}
