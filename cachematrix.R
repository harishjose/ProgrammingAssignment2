
# Assignment: Caching the Inverse of a Matrix
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

##  makeCacheMatrix: This function creates a special "matrix" object that can 
##  cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  m <- NULL
  set <- function( y ) {
    x <<-y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    m <<- inverse
  }
  
  getInverse <- function() {
    m
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Computing inverse of a square matrix can be done with the solve fn in R.
  m <- solve(data, ...)
  
  x$setInverse(m)
  
  m
}
