def nth[T](n: Int, list: List[T]) = {

  def nthInternal(n: Int, head: T, rest: List[T]): T =
    if (rest == null)
      throw new IndexOutOfBoundsException
    else if (n == 0)
      head
    else
      nthInternal(n - 1, rest head, rest tail)

  nthInternal(n, list head, list tail)

}

nth(0, List(1, 2, 3))

nth(2, List(1.3, 3.5, 4))
