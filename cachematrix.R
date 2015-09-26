## These functions cache the inverse of a matrix to prevent
## unnecessary recalculation. Use the makeCacheMatrix 
## function to construct a caching matrix, and use 
## cacheSolve whenever the inverse is needed - the solve
## function will be used to calculate the inverse when
## no cached value is available.
## Example usage is shown below.
# 
#   > 
#   > x<-matrix(c(5,2,3,4),nrow=2)
#   > x
#         [,1] [,2]
#   [1,]    5    3
#   [2,]    2    4
#   >
#   > xc<-makeCacheMatrix(x)
#   >
#   > cacheSolve(xc)
#             [,1]       [,2]
#   [1,]  0.2857143 -0.2142857
#   [2,] -0.1428571  0.3571429
#   >
#   > cacheSolve(xc)
#   using cached value for inverse
#             [,1]       [,2]
#   [1,]  0.2857143 -0.2142857
#   [2,] -0.1428571  0.3571429

## The makeCacheMatrix function constructs a list
## of functions that can be used to get and set
## the raw matrix and the inverse
makeCacheMatrix <- function(x = matrix()) {
  xinverse <- NULL
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) xinverse <<- inv
  getinverse <- function() xinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function checks uses the list of
## functions from makeCacheMatrix to check if the
## inverse has already been calculated, calculate
## and save it if not, and return the result
cacheSolve <- function(x, ...) {
  xinv <- x$getinverse()
  if (!is.null(xinv)) {
    message("using cached value for inverse")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data)
  x$setinverse(xinv)
  xinv
}
