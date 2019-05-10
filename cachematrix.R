## The functions will return the inverse of a matrix. Use the 
## first function to input the matrix and use the second to 
## caculate. And if the matrix remains the same, the second 
## function will cache the result from memory instead of 
## caculating again.

## The first function creates a R object which is a list of 
## functions that stores a matrix and its inverse. When a new
## matrix is put in, the inverse will be assigned as NULL. 
## Double arrow indicates that the assignment should be made 
## to the parent environment, so the value assigned to within 
## the function is accessible after the function ends. 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(x) {
                x <<- x
                m <<- NULL
        }
        get <- function() {
                x
        }
        setsolve <- function(solve) {
                m <<- solve
        }
        getsolve <- function() {
                m
        }
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The second function first check the inverse. If it's NULL then
## caculate and return the inverse. If it's not NULL then skip 
## caculation and retrun the inverse.
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
