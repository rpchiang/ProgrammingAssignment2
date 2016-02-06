## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. This file contains a pair of functions that may be used to
## create an object that caches the inverse of the matrix alongside with the 
## original input matrix.

## This function creates a matrix object
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

## Testing
## tmatrix<-matrix(c(1,2,2,1), ncol=2, nrow=2)
## y  <- makeCacheMatrix(tmatrix)
## z  <- cacheSolve(y)
## z1 <- cacheSolve(y) 
