

## makeCacheMatrix creates and returns a list of functions used by cachesolve to 
## get or set the inverted matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
  ## stores the cache value
  ## initialize to null
  
  m <- NULL
  
  ## create the matrix in the working environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  
  get <- function() x
  
  ## invert the matrix and store it in cache
  
  setinverse <- function(solve) m <<- solve
  
  ## get the inverted matrix from cache
    
  getinverse <- function() m
  
  ## return the created functions
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cachesolve calculates the inverse of the matrix created in makeCacheMatrix 
## if the inverted matrix does not exist in cache
## it is created and stored in cache

cacheSolve <- function(x, ...) {
  
  ## attempt to get inverse of the matrix stored in cache
  
  m <- x$getinverse()
  
  ## return inverted matrix from cache if it exists
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## create the matrix since it does not exist
  
  data <- x$get()
  
  ## set inverted matrix in cache
  
  m <- solve(data, ...)
  x$setinverse(m)
  
  ## display the matrix in the console
  m
  
}
