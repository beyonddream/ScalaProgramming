def fact(n: Int) = {

  def factInternal(n: Int, soFar: Int): Int =
    if (n == 0)
      soFar
    else
      factInternal (n - 1, soFar * n)

  factInternal (n, 1)
}

fact(4)