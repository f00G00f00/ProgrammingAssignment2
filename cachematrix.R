## Put comments here that give an overall description of what your
## functions do

## This code makes the cache matrix object

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


## This function checks to ensure there is a cached inverse value and returns
## it if it does, otherwise it calculates it, caches and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

# Code for testing

# Create a 3x3 matrix filled by columns with values 1 to 9
# my_data <- c(0.1, 0.2, -1.5, 3.2, -0.01, -0.03, 0.5, 0.6, 0.9)
# my_matrix <- matrix(my_data, nrow = 3, ncol = 3)
# print(my_matrix)

# my_inverse = solve(my_matrix)
# print(my_inverse)

# my_cache_matrix = makeCacheMatrix(my_matrix)
# message("my matrix inverse first try:")
# print(cacheSolve(my_cache_matrix))
# message("my matrix inverse first try:")
# print(cacheSolve(my_cache_matrix))
