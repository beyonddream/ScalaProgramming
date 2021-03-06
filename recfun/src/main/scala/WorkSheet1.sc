def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

sum(x => x, 2, 4)

def combinator(f: Int => Int,
               combinator: (Int, Int) => Int,
               zero: Int)
              (a: Int, b: Int): Int = {
  if (a > b)
    zero
  else
    combinator(f(a), combinator(f, combinator, zero)(a + 1, b))
}


combinator(x => x,
  (a: Int, b: Int) => (a + b), 0)(2, 4)

def product(f: Int => Int)(a: Int, b: Int): Int =
  combinator(f, (x, y) => x * y, 1)(a, b)

product(x => x)(1, 4)

def factorial(a: Int): Int = product(x => x)(1, a)

factorial(5)


