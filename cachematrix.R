##makeCacheMatrix and cacheSolve functions allow the efficient inversion
##  of a matrix by attempting to use a cached instance of the result 
##  before solving

##function makeCacheMatrix stores the functions necessary to manage
##  the inverse of the matrix
## set() : updates the input matrix
## get() : returns the input matrix
## setInverse() : sets the inverse matrix
## getInverse() : gets the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  #return a list of functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##function cacheSolve efficiently inverts a matrix by
##  checking the cached version before solving
cacheSolve <- function(x, ...) {
  #attempt to get the inverse
  inv <- x$getInverse()
  
  #get cached version of the inverse, if available
  if(!is.null(inv)) {
     message("getting cached data")
     return(inv)
   }

  #solve the matrix
  data <- x$get()
  inv <- solve(data, ...)
  
  #cache and return the result
  x$setInverse(inv)
  inv
}
