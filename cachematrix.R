## With this assignment, we are creating a pair of functions that
## cache the inverse of a matrix.

## The function below, "makeCacheMatrix," is our first function which will
## will help us to create a special object from a matrix that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix(1:5, 7, 9)) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(x)
    setinverse <- function(inverse)
      inv <<- inverse
  getinverse <- function(inv)
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function, "cacheSolve," will compute the inverse of the reult from
## from our function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("gettig cached datat")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$getinverse(inv)
  inv
}

