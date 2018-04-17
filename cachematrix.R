## Functions for working with a matrix and its inverse


## makeCacheMatrix creates a special matrix that has the ability to cache it's inverse
## getInverse() returns NULL if inverse has not been cached 

makeCacheMatrix <- function(x = matrix()) {
   isset <- NULL
  ## set function assigns a new matrix (y), and removes any stored cache
  set <- function(y) {
   ##re-assign x
    x <<- y
    ##Resets the inverse to NULL. Since the matrix data is new, we will need to recalculate.
    isset <<- NULL
  }
  ## getInverse returns the cached inverse, or NULL if it has not been Cached
  getInverse <-function() isset
  ## setInverse Caches the matrix passed to it as the inverse of the Matrix x
  setInverse <- function(inverse) isset <<- inverse
  ## get returns the value of x
  get <- function() x
  list(set = set, getInverse=getInverse,get=get,setInverse=setInverse)
}



## cacheSolve returns a matrix that is the inverse of the matrix 
##(created by makeCacheMatrix function )passed to it.
## Before it computes the inverse, it checks to see that the inverse has
## not already been cached. If it has been cached, it returns the cached value, 
## otherwise, it calculates the inverse, caches, then returns it.

cacheSolve <- function(x, ...) {
  y <- x$getInverse()
  ## Logical Test to see if inverse of x is cached.
  if(!is.null(y)) {
    ##Inverse of x is cached so return it without recalculating
    return(y)
  }
  ##Inverse of x not previously calculated. Calculate and store the inverse.
  x$setInverse(solve(x$get(),...))
  #Return the newly matrix that is the inverse of 'x'
  x$getInverse()
  
}
