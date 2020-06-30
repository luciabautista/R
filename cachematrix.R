##The purpose of this program in R is to calculate the inverse of a matrix 
##in order to avoid redundant operations and reduce the computational cost.


##makeCacheMatrix builds some functions and returns them within a list.
##This main function creates a matrix object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) s <<- inverse
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve calculates the inverse matrix of the data if they are not
##in memory. On the other hand, the calculus of the inverse is returned if 
## the data are in the memory.

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("the inversed matrix is")
    return(s)
  }
  d<- x$get()
  s <- solve(d, ...)
  x$setinv(s)
  s
}
