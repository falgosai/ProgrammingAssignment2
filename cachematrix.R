## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

maakeCacheMatrix = function(x = matrix()) {
  m = NULL 
  y = NULL 
  setmatrix <- function(y) { 
    x <<- y 
    m <<- NULL 
  }
 
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  if (!is.null(inv)){
 
    message("getting cached data")
    return(inv)
  }

  mat.data = x$get()
  inv = solve(mat.data, ...)

  x$setinv(inv)
  
  return(inv)
}
