## These functions take a matrix object, invert it, then caches that inverted matrix. Further requests to invert the original matrix will generate the cached copy of the inverted matrix, rather than initiate a new invertion function.

## This first function caches the relevant matrix object and its inverse object.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This second function checks for the prior function for the presence of an existing inverse object; if successful, it prints that object. If unsuccessful, it generates the inverse object.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}