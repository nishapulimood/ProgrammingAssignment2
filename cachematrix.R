## The functions makeCacheMatrix and cacheSolve together return the inverse of a matrix, 
## either from a cache where it is stored(using makeCacheMatrix) or 
## by calculating the inverse (cacheSolve). Since the returned value will be the same
## be it retreived from the cache or freshly calculated, the function includes a 
## "message" that is returned alongside the result if that result is 
## retrieved, not calculated.

## makeCacheMatrix creates an object that caches the inverse of a matrix for later retrieval.
## Used in isolation, this function will not return a value. It will only return a value when
##inputted as an argument to cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}


## cacheSolve takes the result of makeCacheMatrix as its argument.
## Then, it either retrieves the matrix inverse from the cache created in makeCacheMatrix
## or calculates it. If the result is retrieved, the output will include a message "retrieved from cache." 
## If the result is calculated, the output will not include any message. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()  
  if(!is.null(i)) {
    message("retrieved from cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
