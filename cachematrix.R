## This R file contains 2 variable functions makeCacheMatrix and cacheSolve. They are to be used together.
## First you call the makeCacheMatrix which takes a matrix as the variable for the constructor function. 
## It assumes that the matrix is a square matrix. It is designed to cache inversed matrix values.
## it provides options for getting the matrix, getting and setting the inverse matrix and listing the functions available.
## Then cacheSolve is used to take in a makeCacheMatrix and use it to determine if the matrix it is setup for has been
## inversed before and if it has return the inverse from storage otherwise return it from cache
##

## makeCacheMatrix function is designed provide getter and setter functions for storing a matrix
makeCacheMatrix <- function(x = matrix()) {
        theMatrixCached <- NULL
        
        ## Store the inversed matrix
        set <- function(y) {
                x <<- y ## This is to store the original matrix for reference
                theMatrixCached <<- NULL ## This is to setup the inverse matrix storage
        }
        
        ## Return the matrix
        get <- function() x
        
        setsolve <- function(inversedMatrix) theMatrixCached <<- inversedMatrix
        
        ## Return the inversed matrix. This will be the inversed value if cached otherwise null
        getsolve <- function() theMatrixCached
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##cacheSolve function is designed to take a makeCacheMatrix
cacheSolve <- function(x) {
        
        ##Get the inversed matrix if it has been inversed previously otherwise null
        m <- x$getsolve()
        
        ## Check if there is already an inverse of the matrix and if there is return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## There is not already an inverse copy of the matrix, inverse the matrix and store it for later reuse
        data <- x$get()
        m <- solve(data) ## invert the matrix
        x$setsolve(m) ## store for later reuse
        m ## return the inversed matrix
}