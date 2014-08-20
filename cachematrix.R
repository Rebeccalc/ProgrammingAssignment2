## These functions are used to compute the inverse of a matrix
## and to store the result in the cache. In this way, the computed result
## will be able to resue in the future analysis without being re-computed.

## Create a special matrix to store the its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinv<-function(solve) m<<-solve
  getinv<-function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Compute the inverse of the matrix and return the result to "makeCacheMatrix" above.
## If the inversion of the matrix has been computed, this function will retrieve the result 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m
}
