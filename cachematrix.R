## makeCache matrix makes a special matrix where inverse is stored if calculated cacheSolve takes the inverse

## takes a matrix use $getinverse to get the inverse and $setinverse to set its inverse

makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL
  set <- function(y) {
    x <<- y
    ix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ix <<- inverse
  getinverse <- function() ix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## if the inverse is not stored it calcualtes and stores the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ix <- x$getinverse()
  if(!is.null(ix)) {
    message("getting cached data")
    return(ix)
  }
  data <- x$get()
  ix <- solve(data, ...)
  x$setinverse(ix)
  ix
}
