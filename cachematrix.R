## This file implements two functions: makeCacheMatrix and cacheSolve

## This function creates a special matrix object.
## This special object is used to store a numeric
## matrix 'x' and and its inverse 'inverse'.
## The function returns a list in which item is a function.
makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL ## Inverse matrix object; set to NULL when creating the special matrix object
        
        ## This function assigns the value of the matrix argument 'y' to 'x'
        ## If the argument is equal to the matrix already stored in 'x', nothing is done
        set <- function(y) {
                if (x != y) {
                        x <<- y
                        inverse <<- NULL
                }
        }
        
        get <- function() x ## Returns the matrix 'x' stored in the special object
        setInv <- function(inv) inverse <<- inv ## Assigns the argument 'inv'to the 'inverse' matrix
        getInv <- function() inverse ## Return the matrix 'inverse'
        
        
        list(set = set, get = get, setInv = setInv, getInv = getInv) ## Return statement
}


## This function calculates and returns the inverse of the special
## object 'x' created with the function 'makeCacheMatrix'.
## The function first checks if the object 'x' already has
## a value for its inverse. If so, then the inverse is returned.
## If not, the inverse is calculated, stored in the 'x' object and,
## finally, returned.
cacheSolve <- function(x, ...) {
        
        i <- x$getInv()
        
        ## Checks if the inverse has already been calculated
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...) ## Calculates the inverse matrix
        x$setInv(i) ## Sets the inverse matrix in the 'x' special matrix object
        
        i ## Return statement
}
