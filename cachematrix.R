## These functions will make it possible to save computation of the matrix inverse by creating
## a matrix function and cacheSolver function, that caches the inverse of a matrix if it has already been comptued.


## This function will store a matrix and its inverse in its environment
## returns a list so that the env functions can be accessed

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  get <- function() x
  
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  setInv <- function(newInv) inv <<- newInv
  getInv <- function() inv
  
  return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}


## This function returns the inverse of the matrix, if the inverse has already been
## calculate it will return the cached version
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv))
  {
    print("using cache")
    return(inv) 
  }
  
  data <-x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}




