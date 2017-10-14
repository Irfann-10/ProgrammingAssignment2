## makeCacheMatrix creates a special "matrix", which is really a list containing a function
## to set the value of the matrix, to get the value of the matrix, to set the value of the matrix and 
## to get the value of the matrix

## The function which is given below, creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   n <- NULL
  set <- function(g) {
    x <<- g
    n <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) n <<- solve
  getsolve <- function() n
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function computes the inverse of the special "matrix" returned
## by make CacheMatrix above

cacheSolve <- function(x, ...) {
        n <- x$getsolve()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setsolve(n)
  n
}
