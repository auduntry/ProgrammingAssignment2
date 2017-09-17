## These functions calculates the inverse of a matrix
## and stores it to the cache so that it can easily and 
## quickly be accessed for subsequent calculations. 

## The makeCacheMatrix() function creates an R object
## that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve() function retrieves the inverse
## matrix from the cached value stored in the 
## makeCacheMatrix() object's environment. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
