# functions for computation matrix inverse and restoration from cache 

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv. <- function(inverse) inv <<- inverse
  getinv. <- function() inv
  list(set=set, get=get, setinv.=setinv., getinv.=getinv.)
}

# cacheSolve function computed the inverse of matrix ifit was not computed before.
# If the matrix inverse was computed beforehand, cacheSolve will restore instead computing it.

cacheSolve <- function(x, ...) {
  inv <- x$getinv.()
  if(!is.null(inv)) {
    message("restore from cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv.(inv)
  inv
}

# sample run results
# x <- matrix(c(12,52,36,54,85,74,69,52,69,54,21,35,652,458,745,856), 4,4)
# z <- makeCacheMatrix(x)
# cacheSolve(z)
              [,1]         [,2]          [,3]         [,4]
[1,] -0.0171815952  0.019343879 -4.773509e-03  0.006891551
[2,] -0.0022824415  0.009993911  2.704424e-02 -0.027146054
[3,]  0.0142436345  0.002594514 -3.227744e-02  0.015854618
[4,]  0.0006401471 -0.001933482 -2.198714e-05  0.001734275
# cacheSolve(z)
# restore from cache
              [,1]         [,2]          [,3]         [,4]
[1,] -0.0171815952  0.019343879 -4.773509e-03  0.006891551
[2,] -0.0022824415  0.009993911  2.704424e-02 -0.027146054
[3,]  0.0142436345  0.002594514 -3.227744e-02  0.015854618
[4,]  0.0006401471 -0.001933482 -2.198714e-05  0.001734275
