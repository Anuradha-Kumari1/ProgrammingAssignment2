## Creating Cache for Solving Inverse Matrix

## The function takes a matrix as an input and acts as a cache for its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <<- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y  
    i <<- NULL
  }
  
  # get the value of the matrix
  get <- function () x
  
  # set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  # get the value of the inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Calculates the inverse of a new matrix inputted in the above function and pulls the inverse for an already calculated matrix

cacheSolve <- function(m,...){
  
  # get the cached inverse
  i <- m$getinverse()
  if(!is.null(i)){
    message("getting from cache")
    return (i)
  }
  
  # if new matrix, calculate the inverse
  data <- m$get()
  i <- solve(data)
  
  # cache the value of the inverse
  m$setinverse(i)
  i
}