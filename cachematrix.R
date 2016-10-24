## Matrix Inversion is often a costly computation and there may be some benefit of caching
## the inverse of the matrix

## Pair of functions that cache the inverse of a matrix

##makecachematrix this function creates a special "matrix" object that can cache
##its inverse

makeCacheMatrix <- function(x = matrix()) {
  invr <-NULL
  set <-function(y){
    x <<- y
    invr <-NULL
  }
  get <- function()x
  setinverse <-function(inverse) invr <<-inverse
  getinverse <-function() invr
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse
  )
}

## This function calculates/solves the inverse of the special matrix
##created in the function above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invr<-x$getinverse()
  if(!is.null(invr)){
    message("getting cached data")
    return(invr)
  }
  data<-x$get()
  inv <-solve(data, ...)
  x$setInverse(invr)
  invr
}

