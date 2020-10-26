## Assignment: Caching the Inverse of a Matrix
## 1. Function makeCacheMatrix: 
## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  reverse <- NULL
  set <- function(y) {
    x <<- y
    reverse <<- NULL
  }
  get <- function() x
  setInverse <- function(ProgAssign) reverse <<- ProgAssign
  getInverse <- function() reverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## 2. Function cacheSolve: 
## Computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  reverse <- x$getInverse()
  if (!is.null(reverse)) {
    message("getting cached data")
    return(reverse)
  }
  data <- x$get()
  reverse <- solve(data, ...)
  x$setInverse(reverse)
  reverse
}