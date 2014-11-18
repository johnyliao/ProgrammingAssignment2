## This program will cache inverse matrix solutions
## for optimization purposes such that matrix inversion
## operations are not duplicated.

## makeCacheMatrix will create a wrapper around a matrix
## data structure which will allow the cacheSolve function
## to query if an inverse matrix operation has been called
## before and return the cache result if it has.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  get <- function() x
  
  
  setInvMatrix <- function(m) invMatrix <<- m
  getInvMatrix <- function() invMatrix
  
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
  
}


## cacheSolve operates along with the cacheMatrix data structure
## such that if an you performed a matrix
## inverse operation on this cacheMatrix, the resuts will be
## cached and repeat calls to matrix inverse operation 
## will pull results from the cache and not repeat calculation
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getInvMatrix()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setInvMatrix(mat)
  mat
}

# Example usage :
# A <- matrix(c(2,2,4,2,1,1,3,1,2),nrow=3)
#
# B <- makeCacheMatrix(A)
# This following line of code will perform an inverse matrix operation
# cacheSolve(B)
#
# This following line of code will pull answer from cache
# cacheSolve(B)
