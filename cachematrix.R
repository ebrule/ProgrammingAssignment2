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
  m <- NULL                                       ##initialize the value of 'm' within local scope
  set <- function(y) {
    x <<- y                                       ##initialize the value of the global variable 'x'
    m <<- NULL                                    ##initialize the value of the global variable 'm'
  }
  get <- function() x                             ##gets the matrix from the global variable 'x'
  setCacheMatrix <- function(solve) m <<- solve   ##sets the value of the global variable 'm'
  getCacheMatrix <- function() m                  ##gets the value of the global variable 'm'
  list(set = set, get = get,                      ##lists methods for function
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
  m <- x$getCacheMatrix()                           ##get the cached value of the solved matrix
  if(!is.null(m)) {                                 ##if the cached value is not null, return the cached value
    message("getting cached data")
    return(m)
  }
  data <- x$get()                                   ##if the cached value is null, get the original matrix
  m <- solve(data, ...)                             ##solve the identity matrix
  x$setCacheMatrix(m)                               ##call a function to set the cached value to the solved identity matrix
  m                                                 ##print the value of 'm' to the screen
}