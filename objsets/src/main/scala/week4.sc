

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat =  new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true
  override def predecessor: Nat = throw new NoSuchElementException("no predecessor")
  override def +(that: Nat): Nat = that
  override def -(that: Nat): Nat = if (that.isZero) this else throw new NoSuchElementException("no unary negation")
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false
  override def predecessor: Nat =  n
  override def +(that: Nat): Nat = new Succ(n + that)
  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}


trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

object List {
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T]() = new Nil
  def apply[T](x1: T): List[T] = new Cons(x1, new Nil)
}


abstract class IntSet {
  //If you have abstract class, then you don't have to have an implementation of class functions
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}


class Empty extends IntSet {
  //contains always false because an empty tree does not contain anything
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def toString = "."
  def union(other: IntSet): IntSet = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if(x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  }
  //creates persistent data structure. If we make changes to the datastructure,
  //we maintain the old copy of the datastructure and make a copy with the changes we want
  def incl(x: Int): IntSet =  {
    if(x < elem) new NonEmpty(elem, left.incl(x), right)
    else if(x > elem) new NonEmpty(elem, left, right.incl(x))
    else this
  }
  override def toString = "{"+left+elem+right+"}"

  //combine this IntSet with the other IntSet. Do not repeat elements

  def union(other: IntSet):IntSet = {
    ((left union right) union other) incl elem
  }
}

val a: Array[NonEmpty] = Array(new NonEmpty(1, new Empty, new Empty))
val b: Array[IntSet] = a
b(0) = new Empty
val s: NonEmpty = a(0)