## Given a nxn invertible matrix x (i.e. x belongs to the general linear group) we want to
## create a program with which we can access its inverse once it has been computed previously.
## By doing so, we avoid having to calculate it again every time. 

## Such procedure described above can be implemented with the aid of the makeCacheMatrix function.
## As a parameter we have the matrix itself and it returns a similar statement to the vector example
## a list containing 4 functions. The first one to set its value, the second one to get its entries, 
## the third one to set its inverse and the fourth to get its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## i stands for the inverse
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  ## set & get do the exact same paper as in the vector example but here with matrices
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Once we have the makeCacheMatrix function we can get the inverse of any given invertible matrix 
## with cacheSolve. cacheSolve checks whether the inverse has been calculated or not. In case it has,
## it simply returns it and otherwise it gets it through makeCacheMatrix's output list.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
