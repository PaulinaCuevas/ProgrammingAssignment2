makeCacheMatrix<-function(m=matrix()){
  inv<-NULL
  set <- function(n) {
    m <<- n
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cachesolve <- function(m, ...) {
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}
