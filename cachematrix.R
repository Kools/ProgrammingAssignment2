## Put comments here that give an overall description of what your functions do
## makeCacheMatrix creates a special "matrix" object 
## and cacheSolve computes inverse of "matrix" if not computed earlier or else returns cached value

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
}
get <- function() x
setInverse <- function(solve) I <<- solve
getInverse <- function() I
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}
## Write a short comment describing this function
# cacheSolve computes the inverse of "matrix" object returned by makeCacheMatrix
# If inverse has been computed before cacheSolve returns cached inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I
}
