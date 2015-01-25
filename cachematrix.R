## Assignment: Caching the Inverse of a Matrix
## Assignment to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      
      # return the "special matrix" which is actually a list containing 
      # setter/getter for the matrix and its inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      # 1. check cache for inverse and return that if available
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("Getting cached data ...")
            return(inv)
      }
      
      # 2. compute the inverse, cache it and return the inverse
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)  # add inverse matrix to cache
      inv
}
