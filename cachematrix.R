## This R file contains 2 variable functions makeCacheMatrix and cacheSolve. They are to be used together.
## First you call the makeCacheMatrix which takes a matrix as the variable for the constructor function. 
## It assumes that the matrix is a square matrix. It is designed to cache inversed matrix values.
## it provides options for getting the matrix, getting and setting the inverse matrix and listing the functions available.
## Then cacheSolve is used to take in a makeCacheMatrix and use it to determine if the matrix it is setup for has been
## inversed before and if it has return the inverse from storage otherwise return it from cache
##

## makeCacheMatrix function is designed provide getter and setter functions for storing a matrix
makeCacheMatrix <- function(x = matrix()) {
        theMatrixCached <- NULL ## define theMatrixCached
        
        ## Store the inversed matrix
        setMatrix <- function(y) {
                x <<- y ## This is to store the original matrix for reference
                theMatrixCached <<- NULL ## This is to setup the inverse matrix storage
        }
        
        ## Return the matrix
        getMatrix <- function() x
        
        ## Update the stored inverse matrix
        setInverseMatrix <- function(inversedMatrix) {
                theMatrixCached <<- inversedMatrix      
        } 
        
        ## Return the inversed matrix. This will be the inversed value if cached otherwise null
        getInverseMatrix <- function() {
                theMatrixCached
        }
        
        ## list of getters and setters
        list(setMatrix = setMatrix
             , getMatrix = getMatrix
             , setInverseMatrix = setInverseMatrix
             , getInverseMatrix = getInverseMatrix
             )
}

##cacheSolve function is designed to take a makeCacheMatrix object/functions and use it to work out
## if the matrix used to create makeCacheMatrix has been inversed previously and if it has
## get it from storage otherwise invert the matrix and store for later use
## requires makeCacheMatrix value to be passed in
cacheSolve <- function(x) {
        
        ##Get the inversed matrix if it has been inversed previously otherwise null
        iMatrix <- x$getInverseMatrix()
        
        ## Check if there is already an inverse of the matrix and if there is return it
        if(!is.null(iMatrix)) {
                message("DEBUG: inverse matrix was cached previously returning that")
                return(iMatrix) ## return the inverted matrix
        }
        
        ## There is not already an inverse copy of the matrix, inverse the matrix and store it for later reuse
        message("DEBUG: inverse matrix not previously worked out, creating inverse and storing for later use")
        data <- x$getMatrix()
        iMatrix <- solve(data) ## invert the matrix
        x$setInverseMatrix(iMatrix) ## store for later reuse
        iMatrix ## return the inversed matrix
}