## Matrix Caching, Assignment #2
## Here we define two functions
## makeCacheMatrix takes a matrix and returns a new object which can cache
## the result of finding its inverse.  
## cacheSolve: Uses an object created using makeCacheMatrix to either use
## the solve() function to determine its inverse or returns the value
## cached.  Once the solution is found it is cached until a new matrix
## is stored in this variable.


## makeCacheMatrix: This function sets up a matrix so that it can cache
## the value of its inverse.  It does this by taking in a matrix and
## returns a list containing four functions.
##
## set: allows us to change the matrix that is being referenced by
##      this variable.  In doing so it removes the cached copy.
## get: returns the value of m which will contain the cached inverse
##      of our matrix.
## setsolve: Sets the value of m in the parent context to the value
##           passed in as a parameter.
## getsolve: returns the value stored in m this will either be our
##           cached inverse matrix or NULL.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list( get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: This function takes in an object that has been created
## using "makeCacheMatrix" and returns the inverse of the matrix either
## by returning the value stored in m in the environment created in
## makeCacheMatrix or calculating the inverse and then storing it both
## in the local variable m and in the variable m in the environment
## created with makeCacheMatrix using the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
  } 
  else {
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  }
  m
}