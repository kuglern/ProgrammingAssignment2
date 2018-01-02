## the following functions used to create a matrix that can cache its inverse.

## makeCacheMatrix is a function that creates a special "matrix" object 
## which is really a list containing a function to:
## 'set' the value of the matrix
## 'get' the value of the matrix
## 'set' the value of the inversed matrix
## 'get' the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## cacheSolve is a function that computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## this function assumes that the matrix supplied is always invertible.

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

