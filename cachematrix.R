## Coursera
## R Programming Course
## Programming Assignment 2

## `makeCacheMatrix` creates a special "matrix", which is
##  really a list containing a function to
##
##  1.  set the value of the matrix
##  2.  get the value of the matrix
##  3.  set the value of the inverse matrix
##  4.  get the value of the inverse matrix
##
##  It takes as an optional parameter a invertible matrix. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
 

## cacheSolve calculates the inverse matrix of the special "matrix"
## created with the above function. 
## It takes as parameter the value returned by 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    
    ## if the inverse matrix is already cached
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    ## For this assignment we assume data is always invertible, but easily we can verify it with...
    ## if the determinat of matrix <data> is 0 then it's not invertible, return NULL
    ## if (det(data) == 0) {
    ##    message("Matrix non invertible.")
    ##    return(NULL)
    ## }
    
    m <- solve(data, ...)
        
    x$setsolve(m)
    m
}
