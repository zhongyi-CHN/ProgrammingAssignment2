## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ##  Initialise the inverse to NULL when the matrix is first passed 
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL  
    ##  Reset the inverse to NULL when a new matrix is passed
  }
  
  get <- function() {x}  ##  Return the current matrix cached
  
  ##  set the inverse and get the current inverse cached
  setinv <- function(inv) {inverse <<- inv}
  getinv <- function() {inverse}
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  ##  return a list of four functions
}


## This function calculates the inverse of the 
## special "matrix" created with the above function

cacheSolve <- function(x, ...) {
  ##  first check whether the inverse already exist
  inverse <- x$getinv()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)  ##  return the cached inverse if it exists
  }
  
  ##  Otherwise, calculate the inverse directly
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinv(inverse)  ##  cache the calculated inverse
  inverse  ##  return the inverse
}
