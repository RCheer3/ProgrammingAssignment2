## The functions makeCacheMatrix and cacheSolve cache the inverse of a variable
## to increase efficiency


## makeCacheMatrix stores the original matrix in the function get()
## after it's first run through the inverse of the matrix is stored in i through the setinverse() function
## which is then stored in the getinverse() function [function() i]

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve is a function that first passes back the string "getting cached data" if there is already an inverse in getinverse()
##then it takes the original matrix get() and takes the inverse of that using the solve function
##finally it runs the setinverse() function from makeCacheMatrix to store the inverse value in getinverse() and also prints out the inverse
cacheSolve = function(x,...){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}