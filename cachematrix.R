## Creating Cache for Solving Inverse Matrix

## The function takes a matrix as an input and acts as a cache for its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <<- NULL
  set <- function(y) {
    x <<- y  
    i <<- NULL
  }
  
  get <- function () x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Calculates the inverse of a new matrix inputted in the above function and pulls the inverse for an already calculated matrix

cacheSolve <- function(m,...){
  i <- m$getinverse()
  if(!is.null(i)){
    message("geeting from cache")
    return (i)
  }
  
  data <- m$get()
  i <- solve(data)
  m$setinverse(i)
  i
}