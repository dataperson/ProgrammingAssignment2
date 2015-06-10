## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The scope of this code is to allow the caching of the inverse of a matrix.

## The following function creates a special "matrix", containing a function to
## set the value of the matrix, get the value of the matrix, set the value of 
## the inverse matrix, and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the setinv
## function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
