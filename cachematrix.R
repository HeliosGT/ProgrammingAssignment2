## THESE FUNCTIONS MAKE THE INVERSE OF A MATRIX
## USING CACHE

## This function make the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize the inverse matrix
  inverse_matrix <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    x <<- matrix
    inverse_matrix <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the original matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    inverse_matrix <<- inverse
  }
  
  ## Method to get the inversed matrix
  getInverse <- function() {
    ## Return the inverse property
    inverse_matrix
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Solve the inverse of the matrix, taking the cached data if exist

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## If the inverse exists then it is returned
  if( !is.null(m) ) {
    return(m)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
