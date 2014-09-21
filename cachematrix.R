## This function creates a special "matrix" object that can cache its inverse.



## Creates cacheable matrix for inputting to
## cacheSolve() function which sets and gets 
## the cached values

makeCacheMatrix <- function(x = matrix()) {
  
  #Is the input a matrix?
  if (!is.matrix(x)) {
    stop("Please give a matrix")
  }
  
  
  inversemat <- NULL
  
  set <- function(y) {
    x <<- y
    inversemat <<- NULL
  }
  
  get <- function() x
  # Inversing the matrix using build in solve() function in R
  setinverse <- function(solve) inversemat <<- solve
  getinverse <- function() inversemat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Compute the inverse of a cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse

cacheSolve<- function(cachemat, ...) {

  inversemat <- cachemat$getinverse()
  # Is the cached matrix available?
  if(!is.null(inversemat)) {
    message("getting cached inverse matrix")
    return(inversemat)
  }
  # Creating an inverted matrix in case
  # there's no cached matrix available
  data <- cachemat$get()
  inversemat <- solve(data, ...)
  cachemat$setinverse(inversemat)
  inversemat
}




