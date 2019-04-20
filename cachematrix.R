## Put comments here that give an overall description of what your
## functions do
##
## My function computes matrix inversion, cache a result and can retrieve 
## an inverted matrix that has already been calculated, if needed. 
## Thus it helps save time. The function includes two complement functions.

## Write a short comment describing this function
##
## The first function creates an object that enables caching matrix and 
## inversion of this matrix. Its environment contains two objects and four 
## functions. After initializing objects x and m, given functions allow to:
## assingn input argument in the parent environment and NULL value to object m,
## then retrieve x, compute value of object m and retrieve inverted matrix.
## Each function is an element of the list and it is named, so it is easier
## to get access to it.

makeCacheMatrix <- function(x = matrix()) {
m <-NULL
  set <-function(y) {
    x <<- y
    m <<-NULL
  }
  get <- function() x
  setInverse <-function(solve) m <<- solve
  getInverse <-function() m
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##
## The second function executes makeCacheMatrix()function. An object that is output of
## makeCacheMatrix()is an input for cacheSolve() and gives access to parent environment.
## First, function tries to retake an inverted matrix for passed argument from cache.
## If a value is not NULL, function returns this value with information "cached data".
## In case one set another input, m value is cleared, so funcion computes inverted 
## matrix for new data and prints it.


cacheSolve <- function(x, ...) {
m <- x$getInverse()
  if(!is.null(m)) {
    message("cached data")
    return(m)
  }
  data <-x$get()
  m <-solve(data, ...)
  x$setInverse(m)
  m
}
