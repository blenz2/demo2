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