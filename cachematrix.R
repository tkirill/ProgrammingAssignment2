## This module contains functions for caching the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.
##
## The object contains four methods:
##   1. set -- set internal matrix and discard cached inverse
##   2. get -- get internal matrix
##   3. setinverse -- cache inverse of the internal matrix.
##   4. getinverse -- retrieve cached inverse

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    cache <<- inverse
  }
  
  getinverse <- function() {
    cache
  }
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
##
## Checks for cached inverse before calculation and do the followin:
##   * If cached inverse exists returns it and prints a message.
##   * Otherwise calculates inverse, stores it in cache and returns the inverse.

cacheSolve <- function(x, ...) {
  cached <- x$getinverse()
  if (!is.null(cached)) {
    message("getting cached data")
    return(cached)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
