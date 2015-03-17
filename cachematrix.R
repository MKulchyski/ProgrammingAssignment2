## The following two functions are used to create a special object that stores a numeric matrix and cache's its inverse.

##The first function, makeCacheMatrix creates a special matrix object, then 
##cacheSolve calculates the inverse of the matrix.
##If the matrix has already been calculated, it will
##find it in the cache and return it without calculating it again.
 makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##The cacheSolve function returns the inverse of the
##matrix created by the makeCacheMatrix function.
##If the cached inverse is available, cacheSolve retrieves it,
##if not, it calculates, caches and returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data.")
        return(inv)
    } else{
      inv <- solve (x$get())
      x$setInverse(inv)
      return(inv)
    }
}

