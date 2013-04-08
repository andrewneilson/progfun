import math.abs

object exercise {
    def sum(f: Int => Int)(a: Int, b: Int): Int = {
        def loop(a: Int, acc: Int): Int = {
            if (a > b) acc
            else loop(a+1, acc+f(a)) 
        }
        loop(a, 0) 
    }

    def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x*y, 1)(a,b)

    def fact(n: Int): Int = product(x => x)(1, n) 

    def mapReduce(f: Int => Int, combine: (Int,Int) => Int, zero: Int)(a: Int, b: Int): Int = {
      if(a > b) zero
      else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
    }

    val tolerance = 0.0001
    def isCloseEnough(x: Double, y: Double) = abs((x-y) / x) / x < tolerance

    def fixedPoint(f: Double => Double)(firstGuess: Double) = {
      def iterate(guess: Double): Double = {
        val next = f(guess)
        if(isCloseEnough(guess, next)) next
        else iterate(next)
      }
      iterate(firstGuess)
    }

    def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

    def sqrt(x: Int) = fixedPoint(averageDamp(y => x/y))(1)
}
