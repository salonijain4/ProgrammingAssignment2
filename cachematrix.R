## Put comments here that give an overall description of what your
## functions do
##There are 2 functions that are used to solve the purpose of matrix inversion and caching its inverse.

## Write a short comment describing this function

##1. The function makeCacheMatrix() is used to create a special object that stores a matrix.
##It creates creates a special “matrix”, which is really a list containing a function to set the value of the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

##2. The function cacheSolve() is used to cache the inverse of the matrix 
##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. If the inverse has already been calculated, then cacheSolve() should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
