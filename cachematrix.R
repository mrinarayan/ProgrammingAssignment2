#Matrix object created to cache inverse, and unless otherwise calculated, inverse of special matrix computed


#This function create a special object that stores a matrix and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y 
    i <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) {
    i <<- inverse 
  }
  getinverse <- function() i
  list(set = set, get = get, 
  setinverse = setinverse, 
  getinverse = getinverse)
}


##Skips computation if inverse has been calculated, otherwise calculates inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Return a matrix that is the inverse of 'x'

m <- matrix(c(1, 2, 3, 5), nrow = 2)
my_matrix <- makeCacheMatrix(m)
cacheSolve(my_matrix)
