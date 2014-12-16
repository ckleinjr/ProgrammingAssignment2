## These functions cache (makeCacheMatrix)and computes(cacheSOlve)an inverse
## of a matrix

## makeCacheMatrix creates a matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## cacheSolve computes the inverse of a matrix that was returned
## by the makeCacheMatrix function.  If the inverse of a matrix is already
## calculated cachesolve will retrieve the calculation from cache.  

cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatrix(m)
  m
}
