## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(x) {
    x    <<- y
    invx <<- NULL # if new matrix, null the cached invx
  }
  get <- function() x
  setinvx <- function(inv) invx <<- inv
  getinvx <- function() invx
  list(set = set, get = get,
       setinvx = setinvx,
       getinvx = getinvx)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
##
## usage: the input x of cacheSolve() can be created by makeCacheMatrix()

cacheSolve <- function(x) {
## Return a matrix that is the inverse of 'x'
  invx <- x$getinvx()
  if(!is.null(invx)) {
    message("returning cached data")
    return(invx)
  }
  invx <- solve(x$get())
  x$setinvx(invx)
  invx
}
