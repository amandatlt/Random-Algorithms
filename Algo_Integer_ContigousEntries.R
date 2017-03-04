#' testing debugging tools
#' this function checks if given an array and a number, if there are
#' contiguous (consecutive) entries in the array that add up to the number
#' @example array c(0,1,3,4,3) and n = 4 should return TRUE

check_integer <- function(V,x) {
  if (x != V[1] & length(V) == 1) ans <- "FALSE"
  if (x < V[1] & length(V) > 1) {
   ans <- "FALSE"
  }
  if (x == V[1]) {
     ans <- "TRUE"
  }
  if (x > V[1] & length(V) > 1) {
    x_new <- x - V[1]
    V_new <- V[-1]
    ans <- check_integer(V_new,x_new)
    if (ans == "FALSE") {
      V_new <- V[-1]
      ans <- check_integer(V_new,x)
    }
  }
  return(ans)
}

#test
V <- c(1,6,6,3,4,10,2)

print(check_integer(V,6))
print(check_integer(V,17))
print(check_integer(V,50))
