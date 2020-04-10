## JHU Coursera R Programming Assignment #2
## April 2020
## author: rockigo@gmail.com 
##
## create list with methods for get / set of both original matrix
## and its inverse, return the list to parent environment
## note that this technique allows use of $ operator to access
## each function from the list 
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
