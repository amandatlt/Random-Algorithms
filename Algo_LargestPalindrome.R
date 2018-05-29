#' Largest palindrome product
#' A palindrome number reads the same both ways
#' Find the largest palindrome made from the product of two 3-digit numbers
#' The premise is that we can start with the multiplication of largest 3 digit value  - 999 - with each other.
#' The function then subtracts 1 from the value and checks if the subsequent multiple is a palindrome
#' 


### function to compare first n/2 characters with the second

compare_halves <- function (x, midpos) {
  
  if (nchar(x) == 1) {
    return(TRUE)
    break
  }
  
  x <- as.character(x)
  
  #extract first part
  numero <- substr(x, start = 1, stop = midpos)
  
  #extract second part and reverse
  uno_orig <- substr(x, start = midpos + 1, stop = nchar(x))
  uno_rev <- rev(unlist(strsplit(uno_orig, "")))
  uno <- paste(uno_rev, collapse = "")
  
  if (numero == uno) {
      TRUE
  } else {
    FALSE
  }
  
}

### function to test if number is a palindrome

is_palindrome <- function (x) {
  
  midpoint = nchar(x)/2
  
  if (midpoint%%1 == 0) {
    return(compare_halves(x, midpoint))
  }
  else {
    num_vec <- unlist(strsplit(as.character(x), ""))
    x <- paste(num_vec[-ceiling(midpoint)], collapse = "")
    return(compare_halves(x, floor(midpoint)))
  }
  
}

### function to obtain largest palindrome from n digit numbers

largest_palindrome <- function(digits) {
  largest_val <- rep("9",length = digits)
  largest_val <- as.numeric(paste(largest_val, collapse = ""))
  
 j <- largest_val
  res <- FALSE
  
  while (j > 0) {
    i <- largest_val
    while (res == FALSE & i > 0) {
      res <- is_palindrome(i*j)
      if (res == TRUE) {
        print(paste(i*j, "is the largest palindrome")) 
        break
      }
      i = i - 1
    }
    #if (res == TRUE) break
    j <- j - 1
  }
}

### largest palindrome from 2 3-digit numbers
largest_palindrome(3)





