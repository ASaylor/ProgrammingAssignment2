## This pair of functions caches the inverse of a matrix
## and retrieves it from the cache if it exists.

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolution <- function(solve) m <<- solve
  getsolution <- function() m
  list (set = set, get = get,
        setsolution = setsolution,
        getsolution = getsolution)
}


## Calculates the inverse of the matrix if it is not cached;
## Otherwise, retrieves value from cache.

cacheSolve <- function(x=matrix(), ...) {
  #Retrieve solution from cache if available
  m <- x$getsolution()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  #Solve if solution is not available in the cache
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setsolution(m)
  m
}
