## This is a definition of a function which defines 4 helping functions for
## the further evaluation

makeCacheMatrix <- function(x = matrix()) {
  
  invM <- NULL 
  
  ## Create and store the helper functions (set, get, setinverse, getinverse)
  set <- function(y) {
    x <<- y
    invM <<- NULL ## reset the inverseMatrix value 
  }
  get <- function() x
  setinverse <- function(invSet) invM <<- invSet
  getinverse <- function() invM
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function does the job: verifies if we have previouly calculated
## and stored the inverese of the matrix. If yes, it gives back the value from memory
## if not, in calculates the inverse matrix and stores it for later reference and of course
## returns the inverted matrix

cacheSolve <- function(x, ...) {
        
  invM <- x$getinverse()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  
  M <- x$get()
  invM <- solve(M, ...)
  x$setinverse(invM)
  
  invM
}
