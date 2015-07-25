
## This function takes a matrix as input and prepares a list of functions
## that can be used to get and set the matix and its inverse
makeCacheMatrix <- function(x = matrix()) {

  x_inv <- NULL                           ##delete if any previous solutions for sove(x) exists 
  
  set <- function(y) {                    ## function for setting a new matrix and setting previous inverse matrix to null
    x <<- y
    x_inv <<- NULL
  }
  
  get <- function() x                     ## function for getting matrix
  
  setinverse <- function(solve) x_inv <<- solve   ## function for setting inverse matrix
  
  getinverse <- function() x_inv        ## function for getting inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
       
  x_inv <- x$getinverse()               ## get inverse matrix of x
  
  if(!is.null(x_inv)) {                 ## if an inverse matrix exists, return that matrix
    message("getting cached data")
    return(x_inv)
  }
  
  data <- x$get()                       ## if an inverse matrix deosnt exists, calculate the inverse of x
  x_inv <- solve(data, ...)
  x$setinverse(x_inv)
  x_inv
}


