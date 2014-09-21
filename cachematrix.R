## This function creates a special "matrix" object that can cache its inverse.


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

cacheSolve<- function(cachemat, ...) {

  inversemat <- cachemat$getinverse()
  # Is the cached matrix available?
  if(!is.null(inversemat)) {
    message("getting cached inverse matrix")
    return(inversemat)
  }
  data <- cachemat$get()
  inversemat <- solve(data, ...)
  cachemat$setinverse(inversemat)
  inversemat
}




