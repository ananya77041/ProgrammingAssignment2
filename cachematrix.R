## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a "vector" which is a list containing functions to:
# 1. Set the value of the vecotr
# 2. Get the value of the vector
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve checks to see if an inverse of the matrix has been cached and returns without computation if it is.
# If the inverse is not cached, then it calculates it and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
