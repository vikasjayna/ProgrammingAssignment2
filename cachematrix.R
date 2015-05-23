## This code provides two functions using which the inverse of a matrix can be calculated and cached for future use

## makeCacheMatrix
## params: x which is a matrix
## returns: a list of four functions - set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve - checks whether the inverse is already computed and if so does not re-compuete the inverse
## params: x which is an object returned by the makeCacheMatrix function
## returns: the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
