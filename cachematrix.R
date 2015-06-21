## The purpose of the code is to calculate the inverse of a matrix, then catch it rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ##set x is an invertible matrix
  i <- NULL
  
  ## 1.function - set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## 2.function - get the matrix
  get <- function() x
  
  ## 3.function - set the inverse matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## 4.function - get the inverse matrix
  getinverse <- function() i
  
  ## return a list containing function 1-2-3-4.
  ## this is the input to cacheSolve() function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## x is the output of makeCacheMatrix()
  i <- x$getinverse()
  
  ## check if the inverse has already been calculated
  ## get it from the cache and return it without computation
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## if the inverse does not exist, calculates it
  data <- x$get()
  i <- solve(data, ...)
  
  ## sets the value of the inverse
  x$setinverse(i)
  
  ##return the inverse
  i
}