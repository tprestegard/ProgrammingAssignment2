############################################################
## FUNCTION DESCRIPTION:
##   makeCacheMatrix takes an input matrix that we can assume
##   is square and invertible.  It returns a list of functions
##   for accessing and setting the matrix and its inverse.
##
## INPUTS:
##   A - a square matrix that we can assume is invertible.
##       Set to a 1x1 matrix of NA by default.
##
## OUTPUTS:
##   List of functions for getting and setting the matrix
##   and its inverse using lexical scoping.
############################################################
makeCacheMatrix <- function(A = matrix()) {
    
    ## Set inverse matrix A_inv to NULL.
    A_inv <- NULL
    
    ## Functions for getting and setting A.
    ## A$get() returns A.
    get <- function() A
    
    ## A$set(B) sets A to be the matrix B
    ## and the inverse A_inv to be NULL.
    set <- function(B) {
        A <<- B
        A_inv <<- NULL
    }
    
    ## Functions for getting and calculating A_inv.
    ## A$getInv() returns the inverse matrix A_inv.
    getInv <- function() A_inv
    
    ## A$setInv(B_inv) sets the inverse matrix A_inv
    ## to be the matrix B_inv.
    setInv <- function(B_inv) A_inv <<- B_inv
    
    ## Return special matrix: list of functions for accessing
    ## and setting the matrix and its inverse.
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


############################################################
## FUNCTION DESCRIPTION:
## cacheSolve takes in a special matrix produced by
## makeCacheMatrix and calculates the inverse of this matrix
## if it hasn't already been calculated.  If the inverse
## has already been calculated, it loads the inverse matrix
## from the cache.
##
## INPUTS:
##   A   - a square matrix that we can assume is invertible.
##   ... - additional arguments that the user may want to
##         pass to the solve() function.
##
## OUTPUTS:
##   Inverse of input matrix A.
############################################################
cacheSolve <- function(A, ...) {
    
    ## Check if A is square.
    ## Need to load the 'matrixcalc' library to do this.
    ## If A is not square, we throw an error.
    library(matrixcalc)
    if (!is.square.matrix(A$get())) { 
        err_msg <- paste("The input matrix has dimensions",
                         dim(A$get())[1],"x",dim(A$get())[2],
                         "and is not square.",sep=" ")
        stop(err_msg)
    }
    
    ## Get inverse of A.
    A_inv <- A$getInv()
    
    ## If A_inv is NULL that means the inverse hasn't been
    ## calculated yet.  In that case, we need to calculate it.
    if (is.null(A_inv)) {
        ## Calculate the inverse matrix.
        message("Calculating matrix inverse.")
        A_inv <- solve(A$get(), ...)
        ## Store the inverse matrix in the A matrix object.
        A$setInv(A_inv)
    } else {
        ## In this case, the A_inv variable already contains
        ## the inverse matrix.  So we just print a message
        ## for the user.
        message("Getting matrix inverse from cache.")
    }
    ## Return A_inv.
    A_inv
}