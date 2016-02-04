## The two functions cache the result of a matrix inverse operation

## This function create an object that stores the calcucated result
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinverse <- function(invMatrix) m <<- invMatrix
   getinverse <- function() m
   list(set = set, get = get,
        setinverse= setinverse,
        getinverse = getinverse)

}


## This function first checks if there is a cached value. If not, it inverses the matrix
## If there is a cached result, it returns the cached result
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
}
