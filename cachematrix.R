## Put comments here that give an overall description of what your
## functions do

## This function will take a matrix, and return a list, which will contain:
## A function to set the matrix value
## A function to get the matrix values
## A function to set the inverse of the matrix
## A function to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y #this should set x, in the outer context, to y.
    inverse <<- NULL #set inverse to NULL because we might not need to bother.
  }
  
  get <- function() x #returns x (original matrix)
  
  setInverse <- function(aMatrix) inverse <<- aMatrix #push aMatrix into inverse 
  
  getInverse <- function() inverse # returns inverse of x, calculated on set()
  
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse) #return the list of functions

}


## The function below takes the output of makeCacheMatrix, and then:
## Checks if there is an inverse already
##  if there is, returns it
##  else, calculates it, then sets it, then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached inverse")
    return(inverse)
  }
  matrix <- x$get() #load the matrix in
  inverse <- solve(matrix) #invert it
  x$setInverse(inverse) #save it for future use
  inverse #return it
  
}
