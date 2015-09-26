## Programming Assignment 2: Caching the Inverse of a Matrix
## The functions created for this assignment allow the user to create a special
## matrix that can cache its own inverse.  The user can then retrieve the 
## inverse from the cache if it is available or calculate it and store it in the
## cache if it is not.

## makeCacheMatrix:  Takes as input a matrix and returns the special matrix
## that consists of a set of functions to set and get the matrix and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## The inverse is initially defined as NULL.
  i <- NULL
  
  ## Sets the value of the matrix.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Gets the value of the matrix.
  get <- function() x
  
  ## Sets the inverse of the matrix.
  setinverse <- function(solve) i <<- solve
  
  ## Gets the inverse of the matrix
  getinverse <- function() i
  
  ## Returns the list of functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve:  Calculates the inverse of the special matrix.  If this has
## already been calculated and is available in the cache, then it returns the
## cached inverse instead of performing the calculation and prints a message
## indicating that the inverse was retrieved from the cache. This happens
## automatically without any need for the user to specify whether the inverse
## should be calculated or retrieved from the cache.

cacheSolve <- function(x, ...) {
  
## Retrieves the inverse from the cache using the getinverse defined in 
## makeCacheMatrix.
  i <- x$getinverse()

## If the matrix retrieved from the cache was not NULL, return it.  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
## If the retrieved inverse was NULL, calculate the inverse and store it using
## the setinverse function from makeCacheMatrix.  Return the calculated result.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}