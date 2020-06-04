## In makeCacheMatrix, we provide a list of closures (set, get, setinverse, and getinverse)
## which will allow cacheSolve to set a matrix, recall it, store its inverse after calculating it, and return
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  #specify matrix
  set <- function(m2){
    x <<- m2
    inverse <<- NULL
  }
  #recall matrix
  get <- function(){
    x
  }
  #sets inverse of matrix
  setinverse<-function(inverse2){
    inverse <<- inverse2
  }
  #returns inverse of matrix
  getinverse<- function(){
    inverse
  }
  
  #Vector created by parent function, which contains a list that sets the
  #value of the initial matrix, gets the matrix's value, sets the inverse of the
  #matrix's value, and gets the inverse's value
  list(set= set, get= get, setinverse= setinverse, getinverse= getinverse)
  
}


## cacheSolve gets the inverse of the matrix. If it has already been calculated, it returns the cached value.
## Otherwise, it solves for the inverse and returns it. 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    return (inverse)
  }
  else{
    m3 <- x$get()
    inverse <- solve(m3, ...)
    x$setinverse(inverse)
    return(inverse)
  }
}
