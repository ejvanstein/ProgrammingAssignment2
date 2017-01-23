## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  matrixinverse <- NULL                     
  set <- function(y) {                      
    x <<- y
    matrixinverse <<- NULL              
  }
  get <- function() x                           
  setinverse <- function(solve) matrixinverse <<- solve 
  getinverse <- function() matrixinverse        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve<- function(x, ...) {                 
  matrixinverse <- x$getinverse()
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  data <- x$get()                               
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}

## input matrix
##      [,1] [,2]
## [1,]    1   -1
## [2,]    4    6

## inverse should be
##      [,1] [,2]
## [1,]  0.6  0.1
## [2,] -0.4  0.1

## my_matrix <- matrix(data=c(1,4,-1,6),nrow=2,ncol=2)
## test <- makeCacheMatrix(my_matrix)
## cacheSolve(test)

