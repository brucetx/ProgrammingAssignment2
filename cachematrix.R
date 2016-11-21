## The following pair of functions cache the inverse of a matrix.  Caching the inverse
## may reduce run time by eliminating the need to compute the inverse repeatedly.  
## 
## Assumptions:
##    1. the matrix supplied is square
##    2. the matrix supplied is invertible
##
## The first function, makeCacheMatrix, creates a special "matrix" object 
## that can cache its inverse.  This function performs the following operations:
##    1. sets the value of the matrix
##    2. gets the value of the matrix
##    3. sets the value of the inverse matrix
##    4. gets the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) 
    { 
      m <- NULL                                            ## Initialize the matrix object
      get <- function() x                                  ## Get the value of the matrix
      setinverse <- function(inverse) m <<- inverse        ## Set the value of the inverse
      getinverse <- function() m                           ## Get the value of the inverse
      list(set = set, 
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
    }
## 
## The second function, cacheSolve, computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), cachesolve will retrieve the 
## inverse from the cache. Otherwise, the function
##    1. Gets the matrix
##    2. Computes the inverse using the solve function
##    3. Caches the inverse
##    4. Returns the inverse
## 
cacheSolve <- function(x, ...) 
    {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) 
        {                                                  ## Determine whether the inverse is available
          message("getting cached data")                   ## Message
          return(m)                                        ## If it does, return the inverse
        }                                                  ## Otherwise
          data <- x$get()                                  ## Get the matrix
          m <- solve(data, ...)                            ## Compute and cache the inverse
          x$setinverse(m)                                  ## Return the inverse
          m
    }
