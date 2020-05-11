## This pair of functions utilizes lexical scoping in order to avoid
## unnecessarily computing a matrix inversion that has already been done.
## A more simple execution of matrix inversion might just compute the 
## inverse without first checking to see if it is a repeated computation,
## which might become a problem with many or large computations.


## The makeCacheMatrix function defines a special matrix object, the
## elements of which are a series of functions that "get" and "set" the
## values of the matrix to be operated on, and the value of its inverse.

## input: matrix to be operated on, inverse (when called by 'cacheSolve')
## output: outputs the matrix and its inverse (if cached) to 'cacheSolve'

makeCacheMatrix <- function(x = matrix()) {
        m_1 <- NULL
        set <- function(y) {
                x <<- y
                m_1 <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m_1 <<- inv
        getinv <- function() m_1
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function takes the matrix of functions from 'makeCacheMatrix'
## including the original matrix, and outputs its inverse. To do this it first
## checks to see if the inverse has already been calculated and cached, and 
## calculates the inverse if not. 'cacheSolve' calls the get and set functions
## that have been defined in 'makeCacheMatrix' and passed through the x argument.

## input: special matrix of functions
## output: inverse of matrix, indication of whether it was previously cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_1 <- x$getinv()
        if(!is.null(m_1)) {
                message("getting cached data")
                return(m_1)
        }
        data <- x$get()
        m_1 <- solve(data, ...)
        x$setinv(m_1)
        m_1
}
