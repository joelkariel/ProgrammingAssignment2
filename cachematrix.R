## These functions will create a matrix and invert it.

## If the inverse matrix is computed in the first function, then the second
## function will simply retrieve this cached object.

## If not, the second function will compute the inverse of the matrix
## which was created in the first function.

## The first function - makeCacheMatrix - will create a special matrix
## object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      invertmatrix <- function(solve) m <<- solve
      getinvertmatrix <- function() m
      list (set = set, get = get,
            invertmatrix = invertmatrix,
            getinvertmatrix = getinvertmatrix)
}

## This second function - cacheSolve - will return the inverse matrix 
## by retrieving the cached value from the first function OR calculating
## the inverse matrix if there is no cached value from the first function

cacheSolve <- function(x, ...) {
        m <- x[[invertmatrix]]
        
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
      
        data <- x[[get]]()
        m <- solve(data, ...)
        x[[invertmatrix]](m)
        m
}
