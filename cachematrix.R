## Coursera
## R Programming Course
## Programming Assignment 2

## `makeCacheMatrix` creates a special "matrix", which is
##  really a list containing a function to
##
##  1.  set the value of the matrix
##  2.  get the value of the matrix
##  3.  set the value of the inverse matrix
##  4.  get the value of the inverse matrix (cached)
##
##  It takes as an optional parameter an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize inverse matrix properties
    m <- NULL
    
    ## Setting the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Getting the matrix
    get <- function() x
    
    ## Setting (caching) inverse matrix of x
    setInverse <- function(inverse) m <<- inverse
    
    ## Getting (cached) inverse matrix of x
    getInverse <- function() m
    
    ## Return a list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
 

## cacheSolve calculates the inverse matrix of the special "matrix"
## created with the above function. 
## It takes as parameter the value returned by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## if the inverse matrix is already cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Getting the matrix
    data <- x$get()
    
    ## For this assignment we assume data is always invertible, but easily we can verify it with...
    ## if the determinat of matrix <data> is 0 then it's not invertible, return NULL
    ## if (det(data) == 0) {
    ##    message("Matrix non invertible.")
    ##    return(NULL)
    ## }
    
    ## Calculate the inverse matrix of data
    m <- solve(data, ...)
        
    ## Store the result 
    x$setInverse(m)
    
    m  ## return
}
