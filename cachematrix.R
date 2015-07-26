## makeCacheMatrix
## Usage        : makeCacheMatrix(x = matrix())
## Despription  : Set and get matrix x and its inverse
## Arguments    : x = matrix()
## Details      : x has to be invertible
makeCacheMatrix <- function(x = matrix()) {
  xinverse <- NULL
  ## Set matrix 'x'
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  
  ## Get matrix 'x'
  get <- function() x
  
  ## Set inverse of matrix 'x'
  setinverse <- function(xinv) xinverse <<- xinv
  
  ## Get inverse of matrix 'x'
  getinverse <- function() xinverse
  
  ## List of available methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
## Usage        : cacheSolve(x, ...)
## Despription  : Return the inverse of matrix 'x'
## Arguments    : x = makeCacheMatrix()
## Details      : Caching is used
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinverse <- x$getinverse()
  
  ## Return cached value if exists
  if(!is.null(xinverse)) {
    message("getting cached data")
    return(xinverse)
  }
  
  ## Cached value doesn't exist
  ## Get matrix 'x'
  data <- x$get()
  
  ## Compute inverse of matrix 'x'
  xinverse <- solve(data)
  
  ## Set inverse of matrix 'x' for later calls
  x$setinverse(xinverse)
  
  ## Return inverse of matrix 'x'
  xinverse  
}
