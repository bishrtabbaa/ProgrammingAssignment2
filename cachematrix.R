## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix extends the default matrix object with caching functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- solve(x)
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## calculate the inverse of a cacheable matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached inverse solution")
    return (i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
