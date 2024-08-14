## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Create a special matrix object that can store its inverse
  
  inv <- NULL  ## Initialize 'inv' as NULL to store the inverse of the matrix
  
  set <- function(y) {  ## Function to update the matrix
    x <<- y  ## Update the matrix in the parent environment
    inv <<- NULL  ## Reset the cached inverse since the matrix has changed
  }
  
  get <- function() x  ## Function to get the current matrix
  
  setinverse <- function(inverse) inv <<- inverse  ## Function to cache the inverse
  getinverse <- function() inv  ## Function to retrieve the cached inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## Return list of functions for matrix operations
}

cacheSolve <- function(x, ...) {
  ## Calculate and return the inverse of the matrix created by makeCacheMatrix
  
  inv <- x$getinverse()  ## Check if the inverse is already cached
  if (!is.null(inv)) {  ## If cached inverse is available
    message("Using cached inverse")  ## Notify that cached data is being used
    return(inv)  ## Return the cached inverse
  }
  
  data <- x$get()  ## Retrieve the matrix data
  inv <- solve(data, ...)  ## Compute the inverse of the matrix
  x$setinverse(inv)  ## Cache the computed inverse
  inv  ## Return the newly computed inverse
}
