## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a "matrix" object which can cache the result of the "solve" operation
makeCacheMatrix <- function(x = matrix()) {
  cachedResult <- NULL
  set <- function(y) {
    x <<- y
    cachedResult <<- NULL
  }
  get <- function() x
  setsolve <- function(result) cachedResult <<- result
  getsolve <- function() cachedResult
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## is calculating the inverse of the "matrix" or is returning the already
## calculated ("cached") result
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheResult <- x$getsolve()
  if(!is.null(cacheResult)) {
    return(cacheResult)
  }
  data <- x$get()
  result <- solve(data, ...)
  x$setsolve(result)
  result
}