## makeCacheMatrix and cacheSolve are here to cache matrix inverse
## creating such a cache can avoid computing several times a big calculation wasting time

## makeCacheMatrix creates a special "whatever" object here designed to be a square invertible real matrix
## this object has set and get accessors about itself and his inverse.

makeCacheMatrix <- function(x = numeric()) {
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


## cacheSolve returns the inverse of an invertible matrix 
## if not already in the cache, compute it with the "solve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
