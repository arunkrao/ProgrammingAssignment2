## The set of functions in this file allow caching of matrix inverses
## Since inverses of large matrices can take significant computational time,
## it is more efficient to store the inverse the first time it is computed
## and only fetch it from the cache in subsequent requests for the inverse

## This function caches the matrix and its inverse and returns a list 
## of function for getting and setting the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  setMatrix <- function (y)
  {
    x <<- y
    matInverse <<- NULL
  }
  
  getMatrix <- function () x
  
  setInverse <- function (inv)   matInverse <<- inv
  
  getInverse <- function () matInverse
  
  list (s = setMatrix, g = getMatrix, sI = setInverse, gI = getInverse)
}


## cacheSolve returns the inverse for a cached matrix, computing it if 
## its the first time that the inverse of the matrix is being requested

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$gI()
  
  if (!is.null (inv))
  {
    message ("Using the cache")
    return (inv)
  }
  
  inv = x$sI (solve (x$g()))
  inv
}

## A test function to see if the matrix caching functions are working correctly 
## In order to avoid printing a 5000 by 5000 matrix, printing a single computed
## number based on all the rows and columns of the inverted matrix.
## The computed number should be the same in each call and the first call is the
## only one that should take significant time

test <- function ()
{
  mat <- matrix (rnorm (25000000), 5000, 5000)
  cache <- makeCacheMatrix (mat)
  print (system.time (iv <- cacheSolve (cache)))
  print (mean (rowSums (iv)))
  print (system.time (iv <- cacheSolve (cache)))
  print (mean (rowSums (iv)))
  print (system.time (iv <- cacheSolve (cache)))
  print (mean (rowSums (iv)))
  
}

## Below are the test results when I run it on my machine

#   > test ()
#   user  system elapsed 
#   96.44    0.13   96.68 
#   [1] -0.002816375
#   Using the cache
#   user  system elapsed 
#   0       0       0 
#   [1] -0.002816375
#   Using the cache
#   user  system elapsed 
#   0       0       0 
#   [1] -0.002816375
#   > 
