object fac {
    def factorial_old(x: Int): Int = if(x==0) 1 else x*factorial_old(x-1)

    def factorial(x: Int): Int = if(x==0) 1 else factorial(x-1)*x
}
