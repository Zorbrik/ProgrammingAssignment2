##The first function creates a special matrix object that can cache its 
##inverse.cacheSolve function then computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. 

##The function defines the argument with default mode of "matrix"
##Then defines the set function to assign new value of matrix in parent 
##environment. Also, get function returns values of the matrix argument
##"setInverse" assigns value of invm in parent environment
##and "getInverse" gets the value of invm where called.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y){
    x <<- y
    invm <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) invm <<- inverse
  getInverse <- function() invm
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##This function computes the inverse of the special matrix returned by 
##makeChacheMatrix function.If the inverse has already been calculated
##then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  invm <- x$getInverse()
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)
  }
  mat <- x$get()
  invm <- solve(mat,...)
  x$setInverse(invm)
  invm
}

## Return a matrix that is the inverse of 'x'
