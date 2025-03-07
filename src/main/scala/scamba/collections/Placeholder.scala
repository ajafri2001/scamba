package scamba.collections

object $ :
    val o = Placeholder()

    class Placeholder:
        def %(rhs: Int): Intermediate = Intermediate(x => x % rhs)
        def +(rhs: Int): Intermediate = Intermediate(x => x + rhs)

    case class Intermediate(f: Int => Int) extends (Int => Int):
        def apply(x: Int): Int = f(x)
        def ==(rhs: Int): Int => Boolean = x => f(x) == rhs

    // val i = ???
    // val p = ???
