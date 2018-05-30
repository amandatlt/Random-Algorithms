#' algorithm for problem 5 of project Euler
#' find the smallest postive number that is evenly divisible (no remainder) by all of the numbers from 1 to 20
#' solved using the idea that all the numbers have prime factors
#' capturing all combinations of prime factors will return the smallest value
#' 


### find any prime of a number
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

### find all the primes of any given number
find_all_primes <- function(x) {
  v <- numeric()
  i <- 1
  while (x > 1) {
    y <- find_prime(x)
    v[i] <- y
    x <- x/y
    i <- i + 1
  }
  v
}

### find the number of times a factor appears per integer

#find all primes per integer
factors_list <- lapply(seq(2,20,1), function(x) {
  v <- find_all_primes(x)
  v <- cbind(x, v)
})
factors <- as.data.frame(do.call(rbind, factors_list))

#count number of times prime appears per integer
factors$count <- 1
factors <- aggregate(count ~ x + v,factors, FUN = sum )
factors <- factors[order(factors$x, factors$v),]

### identify the largest number of times a prime appears
factors$x <- NULL
factors <- aggregate(count ~ v, factors, FUN = max)

### generate number
number <- 1
for (i in 1:NROW(factors)) {
  factor_value <- factors[[i,"v"]]^factors[[i, "count"]]
  number <- number * factor_value
}
number


