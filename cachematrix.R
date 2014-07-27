## Assignment 2 - Function #1: "makeCacheMatrix"
## Write a short comment describing this function

        # makeCacheMatrix creates a "special" matrix object that can cache that matrix's inverse,
        # however the object does not calculate the inverse matrix, it just saves it inside.
        # After asigning a value to the makeCacheMatrix, it can then return any of the following:
        # set: sets a new value for the matrix.
        # get: returns the matrix stored inside the function.
        # setsolve: saves the value returned by the solve() function (The inverse matrix)
        # getinverse: returns the value of the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        solbe <- NULL
        
        set <- function(y) {
                x <<- y
                solbe<- NULL
        }
        
        get <- function() {
                x
        }
        
        setsolve <- function(solve) {
                solbe <<- solve
        }
        
        getinverse <- function() {
                solbe
        }
        
        list(set = set, get = get, setsolve = setsolve, getinverse = getinverse)
}


## Assignment 2 - Function #2: "cacheSolve"
## Write a short comment describing this function

        # cacheSolve is a function desinged to get the inversed matrix
        # from a "special" object you might remind from earlier (the one 
        # created by makeCacheMatrix); cacheSolve accomplishes this by taking
        # the object as an argument 'x', and checking if the inverse value is 
        # already in the "cache"; If it is, the value is promply returned,
        # if it isn't cacheSolve caculates the inverse matrix contained in 'x', 
        # saves it using the "setsolve" defined in makeCacheMatrix, and then it
        # finally returns the result calculated.

cacheSolve <- function(x, ...) {
        solbe <- x$getinverse()
        
        if(!is.null(solbe)) {
                message("Please wait, acquiring cached data")
                return(solbe)
        }
        
        data <- x$get()
        
        solbe <- solve(data,...)
        
        x$setsolve(solbe)
        
        solbe
}
