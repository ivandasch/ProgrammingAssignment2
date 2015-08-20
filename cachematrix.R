## Put comments here that give an overall description of what your
## functions do

## Creating matrix with cached inverse
## Assuming, that matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solv) inv <<- solv
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## Calculate or get cached inverse of matrix
## Assuming, that matrix is invertible
cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)){
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setsolve(inv)
  inv
}
