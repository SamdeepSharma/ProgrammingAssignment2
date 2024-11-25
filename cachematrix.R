## This function creates a special "matrix" object that can cache its inverse.
## The object provides methods to set and get the matrix and its inverse.
makeCacheMatrix <- function(m = matrix()) {
  
  ## Initialize the inverse property to NULL
  i <- NULL
  
  ## Method to set the matrix and clear the cached inverse
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL  # Clear the cached inverse when a new matrix is set
  }
  
  ## Method to get the current matrix
  get <- function() {
    m  # Return the matrix
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse  # Cache the inverse
  }
  
  ## Method to get the cached inverse of the matrix
  getInverse <- function() {
    i  # Return the cached inverse
  }
  
  ## Return a list of the above methods to interact with the matrix and its inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix created by "makeCacheMatrix".
## If the inverse is already cached and the matrix has not changed, it retrieves
## the inverse from the cache, avoiding redundant computations.
cacheSolve <- function(x, ...) {
  
  ## Get the cached inverse if it exists
  m <- x$getInverse()
  
  ## If the inverse is already cached, return it with a message
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Otherwise, calculate the inverse as it's not cached
  data <- x$get()  # Retrieve the matrix from the object
  
  ## Calculate the inverse of the matrix
  m <- solve(data, ...)  # Compute the inverse
  
  ## Cache the inverse in the matrix object
  x$setInverse(m)
  
  ## Return the computed inverse
  m
}


sample_matrix <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)
cachedMatrix <- makeCacheMatrix(sample_matrix)

inverse1 <- cacheSolve(cachedMatrix)
print(inverse1)

inverse2 <- cacheSolve(cachedMatrix)
print(inverse2)
