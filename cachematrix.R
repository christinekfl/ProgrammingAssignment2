## makeCacheMatrix and cacheSolve are a pair of functions that cache the inverse of a matrix
## as follows:

## This function, makeCacheMatrix, creates a special "matrix" object which can cache 
## its inverse, using a function which can:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
     x <<- y
     m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
     }    

## This function calculates the inverse of the special "matrix" created with makeCacheMatrix.
## It first checks to see if the inverse has already been calculated.  If so, it gets the 
## inverse from the cache and skips the computation.  Otherwise, it calculates the inverse of 
## the matrix and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}

