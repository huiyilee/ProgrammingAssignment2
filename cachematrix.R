## Functions to cache the inverse of a matrix

## First, create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(x) i <<- solve(x)
  getinv <- function()
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## The next function computes the inverse of the special matrix from above.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}
