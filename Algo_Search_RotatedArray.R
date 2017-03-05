#' 
#' this algorithm returns the position of an integer
#' in a rotated array that was sorted in descending order
#' @example the integer 5 in c(12,13,1,2,5,10) is at position 5
#' 

pos_rotatedarray <- function(V, x) {
  s <- length(V)
  split <- floor(s/2)
  split = split + 1
  first <- V[1]
  last <- V[split]
  if (first == x) {
    pos <- 1
  } else if (last == x) {
    pos <- split
  } else if (s > 2) {
    if (first > last) {
      if (x > first) {
        pos <- pos_rotatedarray(V[2:(split - 1)], x)
        pos <- pos + 1
      } else {
        pos <- pos_rotatedarray(V[(split +1):s], x)
        pos <- pos + split
      }
    } else if (first < last) {
      if (x>first & x<last) {
        pos <- pos_rotatedarray(V[2:(split - 1)], x)
        pos <- pos + 1
      } else {
        pos <- pos_rotatedarray(V[(split +1):s], x)
        pos <- pos + split
      }
    }
  } else {
    pos <- 9999
  }
  return(pos)
}

#returns >9999 if not found - need to fix this 