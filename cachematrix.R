

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the value of the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  ## get the value of the inverse of the matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  ## check if the inverse of the matrix has already been calculated
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    ## then return the inverse of the matrix
    return(m)
  }
  ## if the inverse of the matrix has not already been calculated, it gets the matrix returned by makeCacheMatrix
  data <- x$get()
  ## then it calulates its inverse
  m <- solve(data, ...)
  ## set it
  x$setinverse(m)
  ## and then return it
  m
}
