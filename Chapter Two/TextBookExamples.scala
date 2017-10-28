object MyModule {
    def abs(n: Int) : Int = 
        if (n < 0) -n 
        else n

    /* Exercise 2.1: Fibonacci */ 
    def fib(n: Int): Int = {
        def go(first:Int, second:Int, acc: Int) : Int = 
            if (first == n) acc
            else go(first+second, first, acc+first) 
        go(1, 0, 0)
    }
    
    /* Exercise 2.2: isSorted */ 
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        def loop(n: Int): Int =
            if (n+1 >= as.length) Boolean = true
            else if (ordered(n, n+1)) Boolean = false
            else loop(n+1)
        loop(0) 
    }

    def greaterThan(A: Int, B: Int): Boolean = {
        if (A > B) Boolean = true
        else Boolean = false
    }


    def factorial(n: Int):Int = {
        def go(n: Int, acc: Int): Int = 
            if (n <= 0) acc
            else go(n-1, n*acc)
        go(n,1)
    }

    // Example of a Higher-Order Function 
    def formatResult(name: String, n: Int, f: Int => Int) = {
        val msg = "The %s of %d is %d" 
        msg.format(name, n, f(n))
    }

    private def formatAbs(x: Int) = {
        val msg = "The absolute value of %d is %d" 
        msg.format(x, abs(x))
    }

    private def formatFactorial(n: Int) = {
        val msg = "The factorial of %d is %d."
        msg.format(n, factorial(n))
    }

    private def formatFib(n: Int) = {
        val msg = "The Fib of %d is %d."
        msg.format(n, fib(n))
    }

    

    def main(args: Array[String]) : Unit = {
        println(formatResult("aboslute value", -42, abs))
        println(formatResult("factorial is: ", 7, factorial))
        println(formatResult("fibinacci is: ", 5, fib))
        }
}
