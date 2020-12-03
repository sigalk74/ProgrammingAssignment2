## Put comments here that give an overall description of what your
## functions do

## the functions get an invertible matrix
## if the matrix inverse is already calculated than it retrives the value
## from the cached variable in memory
## else it inverts the matrix

## Write a short comment describing this function
## the function receives an invertible matrix as input
## set - defines the function to set the matrix x
## and creates a cached placeholder m for the matrix inverse
## get - returns the matrix x 
## setinv - calculcates the inverse of the matrix with solve()
## getiv - gets the inverse of the matrix
## list - returns a list of all the defined functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## the function receives makeCacheMatrix type matrix as input
## Return a matrix that is the inverse of 'x'
## if m is not null (already cached) the retrieves m
## if m is null (not previously calculated fro this matrix)
## the function calculates the value of m by inverting the matrix x 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
