## These two functions cache the inverse of 
## a matrix

## This function creates a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  
  get <- function() x
  
  setMatInverse <- function(matrix_inverse) matInverse <<- matrix_inverse
  getMatInverse <- function() matInverse
    
  list(set = set, get = get, setMatInverse = setMatInverse, getMatInverse = getMatInverse)
        
}


## This function, calculates the inverse of the matrix
## returned by the preceding function (makeCacheMatix()).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  matInverse <- x$getMatInverse()
  
  ## We are checking if the inverse is already calculated.
  ## If it's already calculated, the function returns
  ## the cached inverse matrix.
  if(!is.null(matInverse)) {
    message("Getting cached data")
    return(matInverse)
  }
  
  ## If the inverse is not yet calculated
  ## We store the not yet inversed matrix ("x") in the variable "data".
  data <- x$get()
  
  ## And we are calculate the inverse of that matrix
  ## with the function solve()
  matInverse <- solve(data, ...)
  
  ## Here we are setting the inverse in the cache
  x$setMatInverse(matInverse)
  
  ## The inverse matrix is then returned
  matInverse
}
