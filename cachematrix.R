#Create matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    
      x <<- y
      
      inverse <<- NULL
      
  }
  
  get <- function() x
  
  setinv <- function(inv) inverse <- inv
  
  getinv <- function() inverse
  
  list(set = set, get = get,
       setinv = setinv, getinverse = getinverse)
  
}

#solve for the inverse of the matrix stored above if it is not already available

cacheSolve <- function(x, ...) {
  
  inverse <= x$getinv()
  
  if(!is.null(inverse)) {
    
      message("Retrieving cached data")
    
      return(inverse)
    
  }
  
  matrix <- x$get()
  
  inverse <- solve(matrix, ...)
  
  x$setinv(inverse)
  
  inverse
  
}
  
