##Below the functions are used to create a special object that stores a matrix and caches its inverse. 

## This function receives a special matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets the inverse matrix of x from the cache, if it has been already calculated. 
## Else, the function calculates the inverse of the special matrix and caches it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      i <- x$getinverse()
      if(!is.null(i)) {
        message("getting cached data")
        return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
