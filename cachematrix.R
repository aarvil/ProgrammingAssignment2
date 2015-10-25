## The goal of these functions is to store a matrix 
## and it's inverse to avoid unecessary computation 
## every time a inverse of the matrix is required. 

## This function takes a matrix as an input and caches it's inverse. 

makeCacheMatrix <- function(x = matrix()) {
      
      inverse <- NULL
      set <- function(y) {
            x <<- y                 ## set new Matrix data.
            inverse <<- NULL        ## reset the matrix inverse 
                                    ## to null as new data was submitted
      }
      get <- function() x           ## return stored Matrix data.
      
      setinverse <- function(inv) inverse <<- inv     ## Save inverse in cache
      getinverse <- function() inverse    ## Seturn cached inverse
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)       ##list with object methods

}


## This function calculates the inverse of a given matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()           ##get data from cache
      if(!is.null(inverse))               ##if cache contains data, return cache
      {
            message("returning cached data")
            return(inverse)               ##Return inverse
      }
      data <- x$get()                     ## Get data
      inverse <- solve(data)              ## Calculate inverse
      x$setinverse(inverse)               ## Cache inverse
      inverse                             ## Return inverse.
}
