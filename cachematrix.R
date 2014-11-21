## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL                       # i is the inverse and is initially set to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }                               # the set function sets the inverse to NULL and puts the input y as new object
  get <- function() x             # returns the matrix x
  setinverse <- function(inverse) i <<- inverse       # this sets the input as the inverse
  getinverse <- function() i                          # this returns the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)                       # return a lists of the internal functions so a calling function knows how to access this internal functions
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()                    # get the inverse of the matrix x
  if(!is.null(i)) {
    message("getting cached data")
    return(i)                            # check whether the inverse exists and if not return the cached value
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(i)
  i                                      # if the inverse does not exist the inverse is calculated, set, and returned
  
}
