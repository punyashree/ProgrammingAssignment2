## cachematrix.R
##
## Create a cache matrix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.
##
## Usage:
##  M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix value
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setInverse<- function(inv_input) inv <<- inv_input
  # get the value of the inverse
  getInverse<- function() inv
  
  # return a list of all the above functions
  list(set = set, get = get,
       setInverse= setInverse,
       getInverse= getInverse)
  
}


## The following function calculates the inverse of the special 
## "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setInverse function.

cacheSolve <- function(x, ...) {
  # check if the inverse is already cached,
  # if so, we get the inverse from the cache directly
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  # else, we first get the matrix
  data <- x$get()
  # and calculate the inverse
  inv <- solve(data, ...)
  # next, cache the inverse of the matrix
  x$setInverse(inv)
  # and finally, return the result
  inv
}