## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Create multiple functions 

makeCacheMatrix <- function(x = matrix()) {
  
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinverse <- function(invSet) invM <<- invSet
  getinverse <- function() invM
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
