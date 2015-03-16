## Program Description: Efficient method to determine if the inverse of a
##    matrix has already been determined. If yes, return the cached inverse.
##    Otherwise, calculate and return the inverse.
## Fuction Descriptions:
##   - makeCacheMatrix: Create a matrix object that can cache its inverse.
##   - cacheSolve: Calculate and return the inverse using makeCasheMatrix. 

## Function to create a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL  
  }
  # Create formulas to set and get the matrix and the inverse of the matrix.
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Function to calculate the inverse of the matrix x (from makeCacheMatrix).
## If inverse has already been calculated for the matrix, return cached value.
## Otherwise calculate the inverse from scratch.
cacheSolve <- function(x, ...) {
  ## If inverse already exists, return the cached inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Otherwise return a matrix that is the inverse of 'x'  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
