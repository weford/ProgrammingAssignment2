
## NOTE: Will only work for Invertible matrix
## This function will create a matrix object 
## capable of storing its inverse by calling 
## cacheSolve() on it.
## Its has the following methods:
## get()- returns the original matrix
## set()- sets the original matrix
## getInverse()- returns the inverse of matrix
## setInverse()- sets the inverse of matrix

makeCacheMatrix <- function(x= matrix()) {
  inv <- NULL
  #Start-----------------
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(x){
      inv <<- x
  } 
  getInverse<- function() inv 
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  #End-------------------
}


## NOTE: Will only work for Invertible matrix
## This function will return the Inverse of an
## makeCacheMatrix object. If the inverse has not
## been calculated, it will callculate it using solve()
## then store it in the object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Start------------------
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached Inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  #End--------------------
}
