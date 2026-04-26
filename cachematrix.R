## This set of functions implements a caching system for matrix inversion. Note
## that inverse only work with square matrices.

## The makeCacheMatrix function creates a special matrix object that can store
## a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function () x
   setinverse <- function(inv) m <<- inv
   getinverse <- function () m
   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix if it has not 
## been computed before, or retrieves the cached inverse if it is already 
## available, avoiding repeated calculations.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      
      if (!is.null(m)) {
         message("getting cached data")
         return(m)
      }
      
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
