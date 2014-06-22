## This function creates a special "matrix" object that can cache its inverse.
## The function has 4 methods:
##     set: sets the value of an identity matrix in the global environment - used to initialize global veraibles
##     get: gets the value of an identity matrix from the global environment
##     setCacheMatrix: set the value of a solved identity matrix in the global environment
##     getCacheMatrix: get the value of a solved identity matrix from the global environment
## Sample usage: 
##    m <- makeCacheMatrix()
##    m$set(matrix(c(0,2,2,0),2,2))
##    m$get()
##    m$setCacheMatrix(a)
##    m$getCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCacheMatrix <- function(solve) m <<- solve
  getCacheMatrix <- function() m
  list(set = set, get = get,
       setCacheMatrix = setCacheMatrix,
       getCacheMatrix = getCacheMatrix)
}


## This function computes the inverse of the special "matrix" 
##     returned by makeCacheMatrix(). 
##If the inverse has already been calculated (i.e. the cached value is not null), 
##     then cacheSolve will retrieve the inverse from the cache.
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getCacheMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCacheMatrix(m)
  m
}