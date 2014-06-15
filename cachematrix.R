## In this file are defined two function
## 1) makeCacheMatrix which returns a matrix with methods
## 2) cacheSolve which manages the inverse a matrix with methods


## makeCacheMatrix returns an object matrix with 
## some setters and getters methods for managing  
## its values and inverse
makeCacheMatrix <- function(x = matrix()) {
    x_1 <- NULL
    set <- function(y) {
        x <<- y
        x_1 <<- NULL
    }
    
    set_ij <- function(value, i, j){
        if(i > nrow(x) | j > ncol(x)) {
            return(message("i or j out of bounds"))
        }
        x[i, j] <<- value
        x_1 <<- NULL
    }
    
    get <- function() x
    setsolve <- function(x1) x_1 <<- x1
    getsolve <- function() x_1
    
    list(set = set, get = get, set_ij = set_ij,
        setsolve = setsolve, 
        getsolve = getsolve)
}


## cacheSolve returns the cached inverse for the matrix x 
## if x$getsolve() is not null otherwise 
## calculates the inverse and caches it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_1 <- x$getsolve()
    if(is.null(x_1)){
        message("calculating inverse")
        x_1 <- solve(x$get())
        x$setsolve(x_1)
    }
    x_1
}
