# demo from harvard youtube watched 06/26/25

fibonacci <- function(n) {
    c(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 
      987, 1597, 2584, 4181)[n]
}

library(assertthat)
assert_that(fibonacci(1) == 0)
assert_that(fibonacci(2) == 1)

for (n in 3:20) {
    assert_that(fibonacci(n) == fibonacci(n-1) + fibonacci(n-2))
}

# no errors on this test means it passed! 

# step 2: recursively calculate Fibonacci numbers
fibonacci_recursive <- function(n) {
    if (n == 1) {
        return(0)
    } else if (n== 2) {
        return(1) 
    } else if (3 <= n) {
        return(fibonacci_recursive(n-1) + fibonacci_recursive(n-2))
        }
}
# test
assert_that(fibonacci_recursive(1) == 0)
assert_that(fibonacci_recursive(2) == 1)

for (n in 3:20) {
    assert_that(fibonacci_recursive(n) == fibonacci_recursive(n-1) + fibonacci_recursive(n-2))
}