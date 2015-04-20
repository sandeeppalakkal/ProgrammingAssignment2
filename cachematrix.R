## These functions are written as a part of assignment 2 in R Programming course
## on coursera.
## These functions illustrate the concept of "caching" a variable in R in order 
## to avoid computing it again so as to save time.
## The following functions create a matrix, compute its inverse and caches it 
## for "future" use. In case the inverse is asked to be computed again, if the 
## matrix is not changed from the previous call, the cached result is returned.
## Otherwise, the inverse is computed afresh and returned.

## This function creates a "special" matrix. It returns a list of 4 functions to
## set a matrix to given input, return the matrix, set its inverse matrix to a
## given input and return the inverse matrix, respectively.

makeCacheMatrix <- function(x = matrix()) {
    # Ensure that the given input is a square matrix
    if(is.matrix(x)){
        if(nrow(x) != ncol(x)){
            stop("The input matrix is not square! Exiting ....")
        }
    }else{
        stop("The input is not a matrix!. Exiting ....")
    }
    # Set the inverse matrix TO NULL
    xinv <- NULL
    
    # Function to initialize the matrix x
    setMatrix <- function(y){
        # Initialize the matrix
        x <<- y
        # Reset the inverse to NULL since x changed by this function
        xinv <<- NULL
    }
    
    # Function to return the matrix x
    getMatrix <- function() x
    
    # Function to set the inverse matrix to computed value given as input
    setInverseMatrix <- function(y) xinv <<- y
    
    # Function to return the inverse matrix
    getInverseMatrix <- function() xinv
    
    # Return the list containing the functions
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## This function returns the inverse of the "special" matrix created by the 
## function makeCacheMatrix. If this function was already called for the same
## matrix, then it returns the inverse already cached. Saves time this way.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Get the current inverse matrix
    xinv <- x$getInverseMatrix()
    # Check if the inverse matrix is computed or not. If already computed, then 
    # return the current value. Else go ahead and compute.
    if(!is.null(xinv)){
        # Display message to the command shell
        message("Matrix inverse already computed. Returning the cached data.")
        # Return the current inverse matrix and exit the function
        return(xinv)
    }
    # If inverse matrix is not computed already, proceed henceforth
    
    # Get the matrix
    A <- x$getMatrix()
    # Compute the inverse of the current matrix
    xinv <- solve(A)
    # Set the inverse matrix to the computed value in the cache
    x$setInverseMatrix(xinv)
    # Return the computed inverse matrix
    xinv 
}