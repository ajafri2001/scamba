package scamba.collections

import scala.annotation.tailrec

sealed trait list[+T]:
    def apply(index: Int): T
    def head: T
    def tail: list[T]
    def isEmpty: Boolean
    def length: Int
    def reverse: list[T]
    infix def as[S](f: T => S): list[S]
    infix def where(f: T => Boolean): list[T]
    def ::[S >: T](elem: S): list[S] = new ::(elem, this)
    def ++[S >: T](anotherList: list[S]): list[S]

object list:
    def apply[T](xs: T*): list[T] =
        xs.foldRight(Nil: list[T]) { (h, t) => ::(h, t) }

case object Nil extends list[Nothing]:
    def apply(index: Int): Nothing = throw new NoSuchElementException
    def head: Nothing = throw new NoSuchElementException
    def tail: list[Nothing] = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def length: Int = 0
    def reverse: list[Nothing] = Nil
    def as[S](f: Nothing => S): list[S] = Nil
    def where(f: Nothing => Boolean): list[Nothing] = Nil
    override def ::[S >: Nothing](elem: S): list[S] = new ::(elem, this)
    override def ++[S >: Nothing](anotherList: list[S]): list[S] = anotherList

case class ::[+T](val head: T, val tail: list[T]) extends list[T]:

    def isEmpty: Boolean = false

    override def toString: String =
        @tailrec
        def toStringTailRec(remaining: list[T], result: String): String =
            if remaining.isEmpty then result
            else if remaining.tail.isEmpty then s"$result${remaining.head}"
            else toStringTailRec(remaining.tail, s"$result${remaining.head}, ")
        "[" + toStringTailRec(this, "") + "]"

    def apply(index: Int): T =
        val size = length
        val actualIndex = if index < 0 then size + index else index
        if actualIndex < 0 || actualIndex >= size then throw new NoSuchElementException

        @tailrec
        def applyTailRec(remaining: list[T], currentIndex: Int): T =
            if remaining.isEmpty then throw new NoSuchElementException
            else if currentIndex == actualIndex then remaining.head
            else applyTailRec(remaining.tail, currentIndex + 1)

        applyTailRec(this, 0)

    def length: Int =
        @tailrec
        def lengthTailRec(remaining: list[T], count: Int): Int =
            if remaining.isEmpty then count
            else lengthTailRec(remaining.tail, count + 1)
        lengthTailRec(this, 0)

    def reverse: list[T] =
        @tailrec
        def reverseTailRec(remaining: list[T], accumulator: list[T]): list[T] =
            if remaining.isEmpty then accumulator
            else reverseTailRec(remaining.tail, remaining.head :: accumulator)
        reverseTailRec(this, Nil)

    def ++[S >: T](anotherList: list[S]): list[S] =
        @tailrec
        def concatTailRec(list: list[S], result: list[S]): list[S] =
            if list.isEmpty then result
            else concatTailRec(list.tail, list.head :: result)
        concatTailRec(this.reverse, anotherList)

    def as[S](f: T => S): list[S] =
        @tailrec
        def mapTailRec(remaining: list[T], accumulator: list[S]): list[S] =
            if remaining.isEmpty then accumulator.reverse
            else mapTailRec(remaining.tail, f(remaining.head) :: accumulator)
        mapTailRec(this, Nil)

    def where(f: T => Boolean): list[T] =
        @tailrec
        def filterTailRec(remaining: list[T], accumulator: list[T]): list[T] =
            if remaining.isEmpty then accumulator.reverse
            else if f(remaining.head) then filterTailRec(remaining.tail, remaining.head :: accumulator)
            else filterTailRec(remaining.tail, accumulator)
        filterTailRec(this, Nil)
