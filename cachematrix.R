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

#From Assignment discussion forum, I took unit tests. 
#Results of unit tests from R studio console are as follows.
#> source('C:/git/data_science/ProgrammingAssignment2/cachematrix.R', echo=TRUE)

#> # Following functions cache inverse of a matrix. 
#> # Since computing matrix inverse is an expensive operation,
#> # caching provides benefits as com .... [TRUNCATED] 

#> # This function computes the inverse of the special "matrix" returned by 
#> #  makeCacheMatrix above. If the inverse has already been calculated 
#>  .... [TRUNCATED] 
#> m<-makeCacheMatrix(matrix(1:4,2,2))
#> cacheSolve(m)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(m)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> m$set(matrix(c(1,2,3,6,5,4,9,7,8),3,3,byrow=T))
#> cacheSolve(m)
#           [,1]       [,2]       [,3]
#[1,] -0.5714286 -0.2380952  0.3333333
#[2,]  0.5714286  0.9047619 -0.6666667
#[3,]  0.1428571 -0.5238095  0.3333333
#> cacheSolve(m)
#getting cached data
#           [,1]       [,2]       [,3]
#[1,] -0.5714286 -0.2380952  0.3333333
#[2,]  0.5714286  0.9047619 -0.6666667
#[3,]  0.1428571 -0.5238095  0.3333333
#> 
