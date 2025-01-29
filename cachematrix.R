## These functions reduce the computational cost 
## of repeated access to a matrix inversion

## this function creates a special 'matrix' object
## this matrix object can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}

## this computes or retrieves the inverse
## of the 'special' matrix returned function return above
cacheSolve <- function(x, ...) {
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
