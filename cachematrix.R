## makeCacheMatrix creates a special "matrix" of functions which can cache 
## its inverse. While cacheSolve computes the inverse of the matrix created
## by makeCacheMatrix. If the inverse has already been calculated, then the
## function retrieves the results from cache

## makeCacheMatrix creates a special "matrix" of functions which can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## While cacheSolve computes the inverse of the matrix created
## by makeCacheMatrix. If the inverse has already been calculated, then the
## function retrieves the results from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}
