## This is Assignment No. 2 Lexical Scoping in Coursera - R-Programming Course.
## This program cache the inverse of a function rather than solving it repeatedly.
## The first function cache the matrix, then the second solve the inverse of the cached matrix


## This first function (makeCacheMAtrix) creates a special "matrix" that store the matrix.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function (cacheSolve) solves the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## the comptation below return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
