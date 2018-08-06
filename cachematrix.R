## In order to reduce the time consumed in computation of inverse of matrices
## we create a cache which stores the inverse of the matrix. 

## makeCacheMatrix creates a cache for given matrix.
## Returns lists of functions, set sets the matrix (emptying cache and changing matrix)
## get gets the matrix that we are originally trying  to get the inverse of
## setInverse sets the inverse 
##getInverse gets the inverse

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
 set<-function(y){
   x<<-y
   inv<<-NULL
 }
 get<-function() x
 setInverse<- function(newinv) inv<<-newinv
 getInverse<-function() inv
 list(set = set, get = get,
      setInverse = setInverse,
      getInverse = getInverse)
  
}


## cacheSolve function checks if the inverse is already cahced 
## if yes then returns the chached inverse
## if not then solves the matrix and chaches the inverse then returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting chached data")
    return(inv)
  }
  
  data<-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
  }

