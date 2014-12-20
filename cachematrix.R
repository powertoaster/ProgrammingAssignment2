## Put comments here that give an overall description of what your
## functions do

## Creates an object that can cache the inverse of a matrix and return the cached version on demand.
makeCacheMatrix <- function(x = matrix()) {
  k <- list()  
  
  set <- function(y) {
    x <<- y
    k <- list()  
  }
  
  get <- function() x
  setinverse <- function(x, inverse) {
    key <- paste(x,collapse='')
    k[[key]] <<- inverse
  }
  
  getinverse <- function(x) {
    key <- paste(x,collapse='')
    k[[key]]
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Excercizes the makeCacheMatrix funtion, to cache the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse(x)
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(x,i)
  i
}
