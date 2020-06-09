## Put comments here that give an overall description of what your
## functions do

# The combinaison of the two following functions allows to cache a matrix inverse.

## Write a short comment describing this function
# This function creates a special matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  cached_inverse <- NULL
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cached_inverse <<- inverse
  getInverse <- function() cached_inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# This function takes as input a special matrix created with makeCacheMatrix
# and returns its inverse (computed if not already cached, cached otherwise)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
