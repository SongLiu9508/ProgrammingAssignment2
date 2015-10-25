# function makeCacheMatrix creates a special "vector", which is a list of 4 functions, including
#1.set: setup the matrix
#2.get: get the matrix
#3.setInv: setup the inverse of the matrix
#4.getInv: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  matrixInv <- NULL                           #matrixInv store the inverse of the input matrix
  set <- function(y) {
    x <<- y
    matrixInv <<- NULL                        #if it is a new matrix, matrixInv initialises to null
  }
  get <- function() x                         #return the input matrix
  setInv <- function(mInv) matrixInv <<- mInv #set the inversed matrix
  getInv <- function() matrixInv              #return the inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


#function cacheSolve returns a matrix that is the inverse of 'x', if it is the same matrix, the inversed matrix 
#is returned from cache

cacheSolve <- function(x, ...) {
  
  mInv <- x$getInv()                          #get the inversed matrix from oblect x
  if(!is.null(mInv)) {                        #if it is the same matrix, the inversed matrix is returned from cache
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()                             #if it is a new matrix, get the matrix 
  mInv <- solve(data)                         #matrix inverse
  x$setInv(mInv)
  mInv                                        #return the inverse of the matrix

}
