##This file consists of functions which are used calculate the inverse of a given matrix
## and the result will be stored in a cache. If we need to retrieve the maxtrix inversion 
##result multiple times then instead of calculating the  value of inverse again and again 
## these methods will return the result stored in the cache.


## This method creates a cache matrix and returns a closure (list) which has 
## the data associated with the cache matrix.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse<<-NULL
  
  setmatrix<-function(y)
  {
    x<<-y
    inverse<<-NULL
  }
  getmatrix<-function() x
  setinverse<-function(inv)
  {
    inverse<<-inv
  }
  getinverse<-function() inverse
  
  #closure 
  list(setmatrix= setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function will check the result of  matrix inversion in the cache. If the 
## result is available then it is returned. If the result is not available then
## the matrix inversion is calculated and the result is stored in the cache.

cacheSolve <- function(x, ...) {
  
  ##Get the inverse from the cache
  inv<-x$getinverse()
  
  #If the inverse retrieved from cache is not null
  if(!is.null(inv))
  {
    message("getting cached data")
    ##Return the inverse
    return(inv)
  }
  
  mat<- x$getmatrix()
  
  ##Calculate the inverse 
  inv<-solve(mat)
  
  ##Store inverse in cache
  x$setinverse(inv)
  inv
}
