## These functions are for solving the inverse of a matrix
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinv <- function(solve) ix <<- solve
  getinv <- function() ix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the 
## special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If that is the case, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the matrix and sets the 
## inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ix <- x$getinv()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setinv(ix)
  ix
}
