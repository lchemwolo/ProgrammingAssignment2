## A pair of functions that cache the inverse of a matrix

##makeCacheMatrix does the following:
##-set the value of the matrix
##-get the value of the matrix
##-set the value of the inverse
##-get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function gets the inverse of the matrix created 
##with makeCacheMatrix. It first checks to see if the inverse of 
##an exact same matrix has already been calculated. If so, it gets the 
##inverse from the cache and skips the computation. Otherwise, it calculates  
##the inverse of the data and sets the value of the inverse in the cache 
##via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  v <- x$get()
  ## check if we have done an inverse computation before
  if(!is.null(currMakeCacheMatrix) ) {
    ## get the last matrix used as input to the calculation
    oldv <- currMakeCacheMatrix$get()
    
    ## check if matrix has changed
    if(identical(v,oldv) ) {
      ## get previously calculated inverse
      oldm <- currMakeCacheMatrix$getinverse()
      
      if(!is.null(oldm) ) {
        message("getting cached data")
        return(oldm)
      }
    }else{
      ## matrix has changed
      print(which(v!=oldv))
    }
  }
  
  ## matrix has changed (or it is the first time we are computing an inverse)
  currMakeCacheMatrix <<- x
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
    
  m
}

## create a variable for tracking changes in matrices

currMakeCacheMatrix <- NULL

##Execution example:
##a<-makeCacheMatrix(matrix(c(5,8,3,4), nrow = 2, ncol = 2))
##cacheSolve(a)


