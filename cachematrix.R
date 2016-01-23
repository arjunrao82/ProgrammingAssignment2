## This file contains a pair of function. The first function encapsulates a matrix with storage for its
## cached inverse (along with setters and getters). The second function calculates the inverse of a matrix
## and sets the inverse into the cache, so that no calculation is needed the 2nd time.

## Function encapsulating a matrix with its cached inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function to calculate the inverse of a matrix, along with setting the inverse value into the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

