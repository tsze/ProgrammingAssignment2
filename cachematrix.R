## makeCacheMatrix: This function creates a special "matrix" object that caches its inverse.

## Assign real matrix x to this object
makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    ## Assign NULL to inverse as the matrix changed
    i <<- NULL 
  }
  ## return the real matrix.
  get  <- function() x
  ## store inverse
  setinverse  <- function(inverse) i  <<- inverse
  ## return the inverse of the real matrix.
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of x
cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  ## return matrix if already existing
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## getting the inverse data
  data  <- x$get()
  ## calculate the inverse and set it to the object i
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}
