## These functions invert a matrix and return the inverse values
## If an inverse has already been calculated it is stored in and returned from the cache

makeCacheMatrix <- function(x = matrix()) {
  ## Set the value of the matrix to null
  m <- NULL
  
  ## Create x and matrix in global environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Determine if inverse is already available in cache
  ## If so: return from cache; otherwise calculate inverse and return value
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
