
#Part-I

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }

  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  getInverse <- function() {
    cachedInverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}



##Part -II
cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
    inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

