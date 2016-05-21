## The functions below are designed to compute the inverse of a matrix
## (if it has not yet been computed) and cache the result so that the
## inverse doesn't need to be re-computed. This will save comp time. 

## The makeCacheMatrix function creates a special list containing a
## function that does the following:
## 1. Set the value of the matrix.
## 2. Get the value of the matrix.
## 3. Set the value of the inverse matrix.
## 4. Get the value of the inverse matrix.

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


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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



