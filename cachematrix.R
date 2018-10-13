################################################################################
## Coursera :       Johns Hopkins
## Specialization : Data Science 
## Course #02 :     R Programming
################################################################################
## Programming Assignment 2
################################################################################

# The goal of this assignment is to write an R function, abled to cache 
# potentially time-consuming computations.
# In this Programming Assignment, we will take advantage of the scoping rules 
# of the R language and how they can be manipulated to preserve state 
# inside of an R object.
#
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly 
# This assignment is to write a pair of functions that cache the inverse 
# of a matrix. It is based on the "Caching the Mean of a Vector" example.
#
# A full usage example provided by course mentor : Alan E. BergerMentorWeek
#
# The file : "tests for cachematrix.R" contains the code of the forum thread:
#         "Simple test matrices for the lexical scoping programming assignment"
#         by Alan E. Berger (Mentor)



################################################################################
## Function : makeCacheMatrix
################################################################################ 
# This function buils a "Cache Matrix" from a standard R Matrix.
# This special matrix is in fact a list containing a function to :
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the reverse of the matrix
# - get the value of the reverse of the matrix 
#
# Parameters :
#    Matrix 
#
# Return :
#    A "Cache Matrix"
#

makeCacheMatrix <- function(m = matrix()) {
    # inv : the inverse matrix (cache)
    inv <- NULL
    # store the original matrix and reset the cache
    set <- function(y) {
        # store the original matrix
        m <<- y
        # reset the inverse matrix
        inv <<- NULL
    }
    
    # returns the classic matrix
    get <- function() m
    
    # store the compouted reverse matrix in the cahce (inv variable)
    setinverse <- function(solve) inv <<- solve
    
    # returns the inv variable
    getinverse <- function() inv
    
    # returns the list of function
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



################################################################################
## Function : cacheSolve
################################################################################ 
# This function returns the Reverse matrix of the "Cache Matrix" input parameter
# If the Reverse matrix is in the cache, returns it
# Otherwise, compute the Reverse matrix, cache it, and returns it
#
# Parameters :
#    "Cache Matrix" (created using the makeCacheMatrix function) 
#
# Return :
#    The reverse matrix
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Get the reverse matrix from the cache
    inv <- x$getinverse()
    # Check if the reverse matrix was found in the cache
    if(!is.null(inv)) {
        # the Inverse matrix was in the cahce, returns it
        # display an info message
        message("getting cached data")
        return(inv)
    }
    # The reverse matrix was not in the cache, we need to compute it
    # - Get the Matrix, 
    # - Compute its reverse
    # - Store the reverse matrix in the cache
    # - returns the Reverse matrix
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
