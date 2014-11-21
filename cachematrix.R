## makeCacheMatrix is meant to create a special matrix given an input of a matrix. 
#This special matrix should also cache the inverse of the given matrix.
#Given that in the example the "vector" is actually a list of fuctions, the "matrix" will also be a list.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setinv <- function(y) m <<- y
  getinv <- function() m
  list(get = get <- function() x,
       setinv = setinv,
       getinv = getinv)
}

#The second vector uses the previously identified functions to check if the matrix
#has been solved. If yes, provides the inverse. If not, completes the computation.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("retrieving data")
    return(m)
  }
  else {
    message("Computing the inverse")
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
  }
}