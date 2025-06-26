# demo from harvard youtube watched 06/26/25

# calculate fibonacci method 1
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

# method 2: recursively calculate Fibonacci numbers
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

# method 3 
# define fibonacci with exact formula
fibonacci_exact <- function(n) {
    phi <- (1 + sqrt(5))/2 # this is the golden ratio
    fib <- (phi^(n-1) - (-1/phi)^(n-1))/sqrt(5)
    return(fib)
}


# test
assert_that(fibonacci_exact(1) == 0)
assert_that(fibonacci_exact(2) == 1)

# check method 3
for (n in 3:50){
    assert_that(abs(fibonacci_exact(n) - (
        fibonacci_exact(n-1) + fibonacci_exact(n-2)
    )) < 0.0001)
}

# add visualization
library(tidyverse)

df <- tibble(x = 1:40, y= fibonacci_exact(x))

plot1 <- ggplot(df, aes(x = x, y = y)) + 
    geom_line() +
    xlab("n") +
    ylab("Fibonacci(n)") +
    ggtitle("Fibonacci Numbers (linear scale)")
plot2 <- ggplot(df, aes(x = x, y = y)) +
    geom_line() + 
    scale_y_log10() + 
    xlab("n") +
    ylab("Fibonacci(n)") +
    ggtitle("Fibonacci Numbers (log scale)")

library(patchwork)
plot1 + plot2

ggsave("fibonacci_numbers_figure.png", width=12, height=5)
patchwork()