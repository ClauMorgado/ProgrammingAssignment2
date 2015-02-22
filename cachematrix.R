## Put comments here that give an overall description of what your
## functions do
## The two following functions cache de inverse of a matrix

## Write a short comment describing this function
## The function makeCacheMatrix creates a list that sets the value of the matrix,
## gets its value, sets its inverse and gets its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() {
      x
    }
    setinverse <- function(inverse_matrix) {
      inverse <<- inverse_matrix
    }
    getinverse <- function() {
      inverse
    }
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

}


## Write a short comment describing this function
## The cacheSolve function calculates the inverse of the matrix created with the 
## makeCacheMatrix function. It first checks if the matrix as already been calculated
## and if it as, the function gets the matrix from the cache and skips the calculations.
## If the matrix was not calculated it sets its value via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
      message("getting cached data.")
      return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
