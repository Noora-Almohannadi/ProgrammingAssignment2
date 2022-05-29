## Overall, makeCacheMatrix and cacheSolve cache the inverse of a matrix

## makeCacheMatrix creates a special matrix object that is able to cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function (inverse) m<<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function takes the special matrix created by makeCacheMatrix and 
## calculates its inverse. cachesolve retrieves the inverse from the cache if 
## the inverse has been calculated already.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ... )
  x$setinverse(m)
  m
}
