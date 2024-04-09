## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function to create a special "matrix" object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize a list to store the matrix and its cached inverse
  cache <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL # Clear the cache whenever the matrix is set
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to compute and cache the inverse of the matrix
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  # Function to get the cached inverse if available
  getInverse <- function() cache
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...){
  # Retrieve the cached inverse if available
  inverse <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inverse)) {
    message("Getting cached inverse")
    return(inverse)
  }
  
  # If the inverse is not cached, compute it
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  
  # Cache the inverse
  x$setInverse(inverse)
  
  # Return the computed inverse
  inverse
}
