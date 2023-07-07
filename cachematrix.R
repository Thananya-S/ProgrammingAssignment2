## In this script, we will take an advantage of the scoping rules
## of the R language by create a function that cache the inverse 
## of a matrix rather than computing it repeatedly. 
## This can help to reduce time-consuming computation by matrix conversion.
##**************************************************************************
## The first function is the makeCacheMatrix function. This function
## creates special object that stores a matrix and caches its inverse.
## Assuming that matrix is an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv.mat <- NULL
  set <- function(y){
    x <<- y
    inv.mat <<- NULL
  }
  
  get <- function(){ x }
  setinverse <- function(x){ inv.mat <<- solve(x) }
  getinverse <- function(){ inv.mat }
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##**************************************************************************
## This second function is cacheSolve function. This function computes the 
## inverse of the matrix returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), then
## cacheSolve retrieve the inverse from the cache otherwise computes inverse
## matrix using solve().

cacheSolve <- function(x, ...) {
  inv.mat <- x$getinverse()
  if(!is.null(inv.mat)) {
    message("getting cached data")
    return(inv.mat)
  } else{
    mat <- x$get()
    inv.mat <- solve(mat, ...)
    x$setinverse(mat)
    return(inv.mat)
  }
}