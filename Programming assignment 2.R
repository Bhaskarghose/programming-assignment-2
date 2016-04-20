## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Caching the Inverse of a Matrix:

# Matrix inversion is costly computation 
#There is benefits for 5aching the inverse of a matrix rather than compute it repeatedly. 
#The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates this functions
# 1. set and get the value of the matrix
# 2. set and get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function


# This function returns the inverse of the matrix. 
#This function checks if the inverse is computed. 
#If it is, the function gets the result and skips the computation. 
#If not, it computes the inverse, sets the value in the cache via set inverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}