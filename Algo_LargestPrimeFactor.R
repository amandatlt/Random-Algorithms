#'
#' algorithm for problem 3 of project euler
#' finding the largest prime factor of a number
#' solved with the basis that numbers are always product of primes
#' @example the number 13195 should return 29
#'

# find any prime of a number
find_prime <- function(x) {
  i = floor(sqrt(x))
  while (i > 1) {
    result <- x/i
    success <- ifelse(result - round(result) == 0, TRUE, FALSE)
    if (success == TRUE) {
      x <- find_prime(result)
      break
    } else {
      i = i - 1
    }
  }
  return(x)
}

# find largest prime of any number using find_prime function
largest_prime <- function(x) {
  V <- x
  i = 1
  while (x > 1) {
    y <- find_prime(x)
    V[i] <- y
    x <- x/y
    i = i + 1
  }
  return(max(V))
}

print(largest_prime(600851475143))