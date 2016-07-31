makeCacheMatrix <- function(x = matrix()) {
  # Matrix inversion is usually a expensive computation and there may be some advantages
  # to caching the inverse of a matrix rather than compute it repeatedly. The
  # following two functions are used to cache the inverse of a matrix.
  # makeCacheMatrix creates a list containing a function to
  # 1. set the value of the matrix
  # 2. get the value of the matrix
  # 3. set the value of inverse of the matrix
  # 4. get the value of inverse of the matrix
  
  inv = NULL
  set = function(y) {
 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolved <- function(x, ...) {
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("pulling cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}

