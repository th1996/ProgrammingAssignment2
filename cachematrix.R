## Functions to cache the inverse of a matrix
## functions do

## function makeCacheMatrix creates a special matrix object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  mCache <- NULL
  set <- function(y){
      x <<- y
      mCache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mCache <<-inverse
  getInverse <- function () mCache
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## function cacheSolve computes the inverse of the special matrix returned by
## the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
       invMatrix <- x$getInverse()
       if(!is.null(invMatrix)){
         message("getting cached data")
         return(invMatrix)
       }
       cacheData <- x$get()
       invMatrix <- solve(cacheData, ...)
       x$setInverse(invMatrix)
       invMatrix
}
