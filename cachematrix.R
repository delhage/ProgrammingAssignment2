## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# makeCacheMatrix creates an object from a matrix x which
# includes 4 functions: 
# get() returns the matrix x
# set() sets a new matrix and NULLs the cahed inverse
# setInverse() creates the inverse of x (stored in inverse)
# getInverse() returns inverse (either cached or NULL)
#
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#
# The solver first runs the getInverse() function on the object,
# storing it in inverse. 
# If inverse is not NULL that means that we have a cached inverse
# and return that.
# Otherwise we get the matrix with get(), solve for the inverse
# with solve() storing the result in inverse, and finally we set
# the inverse in the object with setInverse() and return inverse
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat)
  x$setInverse(inverse)
  inverse
}
