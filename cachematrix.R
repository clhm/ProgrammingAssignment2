# Programming assignment 2: use provided function prototypes (from Coursera
# R Programming class, https://github.com/rdpeng/ProgrammingAssignment2)
# to create two functions for creating and accessing an object that caches
# a matrix inverse if/when calculated.  Function contents for
# makeCacheMatrix and cacheSolve were largely modeled after the example 
# functions provided in the assignment instructions: makeVector and 
# cachemean, respectively. 

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Create an enhanced matrix object that will store its inverse.
# Input: a square, invertible matrix.
# Output: a list containing four functions:
#     set: set the matrix vaue
#     get: get the matrix value
#     set_inv: set the matrix inverse value
#     get_inv: get the matrix inverse value
# No laboratory animals were harmed in the making of this function.
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(x_inv) inv <<- x_inv
    get_inv <- function() inv
    list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## Write a short comment describing this function

# Return the inverse of an enhanced matrix object created by makeCacheMatrix.
# Retrieve the stored inverse if available, otherwise compute and store the 
# inverse before returning.
# Input: makeCacheMatrix returned object.
# Output: matrix inverse.
# This function is non-toxic and has no nutritional value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if (!is.null(inv)){
        message("Wahoo! Using stored value.")
        return(inv)
    }
    # Calculate and store if inverse was unavailable.
    inv <- solve(x$get(), ...)
    x$set_inv(inv)
    inv
}
