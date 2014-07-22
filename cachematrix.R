## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" containing functions to
## set and get the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize the value of the inverse
  inv <- NULL
  
  ## define functions for the object
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  
  ## list of functions
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## The cacheSolve function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. It uses cache for performance reasons.

cacheSolve <- function(x, ...) {
  
  ## try to get data from cache
  inv <- x$getinv()
  
  ## if the data found in the cache the cached data returned
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if the data not exists in the cache the inverse computed using the solve function
  data <- x$get()
  inv <- solve(data, ...)
  
  ## save the computed data in the cache
  x$setinv(inv)
  
  inv
}
