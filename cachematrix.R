## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to
## 1. sets the values of the matrix, 
## 2. gets the values of the matrix, 
## 3. sets the values of the inverse of the matrix, 
## 4. gets the values of the inverse of the matrix

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



## The function CacheSolve returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix 
## and sets the value in the cache via setInverse function. 

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
