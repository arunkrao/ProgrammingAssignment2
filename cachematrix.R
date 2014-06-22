## These pair of functions enable caching of matrix inverses so that repeated retrievals of matrix inverses are efficient
## 

## makeCacheMatrix lets us cache the inverse of a matrix so that we do not have to repeatedly spend time computing it

makeCacheMatrix <- function(x = matrix()) {
	xinverse <- NULL
	set <- function (y) {
		x <<- y
		xinverse <<- NULL
	}
	get <- function () x
	setinverse <- function (inv) xinverse <<- inv
	getinverse <- function () xinverse
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks if the inverse of the matrix is available in the cache, if so returns it, else computes, populates ..
##   .. the cache and then returns the inverse

cacheSolve <- function(x, ...) {
 	inv <- x$getinverse ()
	if (!is.null (inv)) {
		message ("getting cached inverse of the matrix")
		return (inv)
	}
	mat <- x$get ()
	inv <- solve (mat)
	x$setinverse (inv)	
	inv
       ## Return a matrix that is the inverse of 'x'
}

## Not part of the functions, test code to check if the above functions work correctly or not
m <- matrix (1:4, nrow = 2, ncol = 2)           # Make a test matrix  ...
m2 <- makeCacheMatrix (m)	                # Create a cache for it ...
cacheSolve (m2)                                 # Get the inverse - first time - should compute & store in cache
cacheSolve (m2)                                 # Get the inverse again - should use the cache this time
