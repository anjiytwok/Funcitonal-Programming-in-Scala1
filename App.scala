object App extends App {

val list = List(1, 2, 4)

def length[A](l: List[A]) : Int = l.foldRight(0)((a,b) => b + 1)
 //type A = 
def sum[A](lst: List[A])(implicit M: Monoid[A]) : A = {
  lst.foldLeft(M.zero)((a,b)=> M.append(a, b))
}

implicit val monoidIn = new Monoid[Int] {
  def zero = 0
  def append(a1: Int,a2: Int) = a1 + a2
}

trait Monoid[A] {
  def zero: A
  def append(a1: A, a2: A): A
}

implicit val stringNumeric = new Monoid[String] {
    def zero = ""
    def append(a1: String, a2: String) = a1 + a2
}

println("value:" + (if (sum(list) == 7) "true" else "false"))

println("value:" + (if (sum(List("Hello", "World")) == "HelloWorld") "true" else "false"))


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
      ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
          simple(seed2))
    }
  }
}

def posInt(rng: RNG): (Int, RNG) = {

    val (pos, n) = rng.nextInt
    if(pos == Int.MinValue) posInt(n)
    else (pos.abs,n)

}
}
