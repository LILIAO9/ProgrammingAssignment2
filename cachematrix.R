#### These functions are designed to cache the inverse of a matrix
#### So that to decrease the time for running the code

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #set the stored value to null
  i <- NULL         
  #set value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the inverse of the matrix
  setsolve <- function(solve) i <<- solve
  #get the inverse of the matrix
  getsolve <- function() i
  #return all functions as a list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
  cacheSolve <- function(x, ...) {
    i <- x$getsolve()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
  }
