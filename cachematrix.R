## These two functions attempt to retrieve the inverse of a matrix
## from a cache, and calculate the inverse of a matrix
## if it does not already exist in the cache

## This function takes a matrix, x, as an argument and returns a list
## containing functions that can cache the inverse of the matrix 
## Setsolve assigns solution of the matrix to a variable in the
## parent environment

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        set <- function(y) {
                x<<- y
                s<<- NULL
        }
        
        get <- function() x
        
        setsolve <- function(solve) s<<- solve
        
        getsolve <- function() s
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function takes a special matrix, x, as an argument
## and returns the inverse of the matrix. First it
## checks to see if an inverse already exists, by calling
## one of the functions (getsolve) created in the other function.
## If an inverse is found, this function returns that inverse.
## If an inverse is not found, this function calculates
## the inverse (s), and stores the inverse in the cache
## for future use, by calling another function (setsolve)
## created in the other function.  Finally the function
## returns the inverse matrix 

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s    
        
}
