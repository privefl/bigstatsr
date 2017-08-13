b <- matrix(data = rnorm(25), nrow = 5, ncol = 5)
dimnames(b) <- list(letters[1:5], LETTERS[1:5])
a <- structure(b, class = "TestMatrix")

dim(a)
dimnames(a)

transform_ind <- function(k, lim) { 
  
  if (is.character(k))
    stop("Character subsetting is not allowed.")
  
  res <- seq_len(lim)[k]
  
  if (any(is.na(res)))
    stop("Subsetting with missing values is not allowed.")
  
  res
}

transform_ind(c(TRUE, FALSE), 5)
transform_ind(1:2, 5)
# transform_ind(1:10, 5)
# transform_ind("A", 5)
# transform_ind(, 5)

extract <- function(extract_vector, extract_matrix) {
  
  transform_ind <- function(k, lim) { 
    
    if (missing(k))
      return(seq_len(lim))
    
    if (is.character(k))
      stop("Character subsetting is not allowed.")
    
    res <- seq_len(lim)[k]
    
    if (any(is.na(res)))
      stop("Subsetting with missing values is not allowed.")
    
    res
  }
  
  function(x, i, j, drop = TRUE) {
    
    n <- nrow(x)
    m <- ncol(x)
    
    nargs <- (nargs() - !missing(drop))
    if (nargs == 2) {
      
      print("k")
      
      if (missing(i)) {
        nargs <- 3
      } else {
        if (is.logical(i))
          stop("Logical vector subsetting is not allowed")
        
        if (!isTRUE(all(i > 0)))
          stop("Only positive vector subsetting is allowed")
        
        if (is.matrix(i))
          i <- (transform_ind(i[, 2], m) - 1L) * n + transform_ind(i[, 1], n)
        
        if (any(i > (n * m)))
          stop("Subscript out of bounds.")
        
        return(extract_vector(x, i))
      }
      
    } 
    if (nargs == 3) {
      
      print("(i, j)")
      
      res <- extract_matrix(x, transform_ind(i, n), transform_ind(j, m))
      
      return(`if`(drop, drop(res), res))
      
    }
    
  }
  
}

Rcpp::sourceCpp('tmp-tests/test-accessor2.cpp')
`[.TestMatrix` <- extract(getVec, getMat)
  


b
a[]
a[1]
a[1:5]
a[, 1]
a[1, ] 
a[1, 1] 
a[1:2, 1] 
a[1:2, 1:2] 
a[cbind(1:2, c(1, NA))] # NOKK
b[cbind(1:2, c(1, NA))]

a[TRUE, ]
a[c(TRUE, FALSE, FALSE, TRUE, TRUE), ]
a[, "A"]
# a[c(TRUE, NA, FALSE, TRUE, TRUE), ]


a[, 1, drop = FALSE]
a[-1]
a[-1, ]
a[25]
a[28]
# a[, 6]
# a[6, ]
a[-(1:5), ]
a[-6, ]
b[-6, ]
b[0]
# a[0]
b[-25]
b[c(NA, 2, 3, NA)]

crochet:::convertIndex(b, c(NA, 2, 3, NA), "k")
crochet:::convertIndex(b, 26, "k")

