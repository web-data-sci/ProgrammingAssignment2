# Following functions cache inverse of a matrix. 
# Since computing matrix inverse is an expensive operation,
# caching provides benefits as compared to repeated computation

# This function creates a special "matrix" object that can cache its inverse.
# It provides a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(data) {
    x <<- data
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
#  makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then it retrieves inverse from cache
# If inverse is not in a cache, it computes the inverse using solve
# and puts it in a cache using setinverse
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
