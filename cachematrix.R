## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y){
    x <<- y
    invs <<- NULL 
  }
  get <- function(){x}
  setInverse <- function(inverse){invs <<- inverse}
  getInverse <- function(){invs}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getInverse()
  if(!is.null(invs)){
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data)
  x$setInverse(invs)
  invs
}


# Sample run:
# x=matrix(1:4,nrow = 2,ncol = 2)
# m = makeCacheMatrix(x)
# m$get()
# cacheSolve(m)
