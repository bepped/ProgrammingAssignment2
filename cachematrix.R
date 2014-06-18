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
 
    # set a value in position (i, j) of the matrix
    set_vij <- function(v = NULL, i = NULL, j = NULL){
        if(is.null(v) | is.null(i) | is.null(j) ) {
            return(message("no argument can be null") )
        }
        if(i > nrow(x) | j > ncol(x)) {
            return(message("i or j out of bounds"))
        }
        x[i, j] <<- v
        x_1 <<- NULL
    }

    # set all values of a row (default) or a column of the matrix
    setline <- function(lin = NULL, k = NULL, row = TRUE) {
        if(is.null(lin) | is.null(k) ) {
            return(message("no argument can be null"))
        }
        if(row){ 
            if(length(lin) != ncol(x) | k > nrow(x))
                return(message("arguments out of bounds"))
            
            x[k,] <<- lin
        }
        else {
            if(length(lin) != nrow(x) | k > ncol(x) )
                return(message("arguments out of bounds"))
            
            x[,k] <<- lin
        }
        x_1 <<- NULL
    }
    
    # set a column of the matrix
    setcol <- function(c = NULL, j = NULL) {
        setline(c, j, FALSE)
    }
    
    #set a row of the matrix
    setrow <- function(r = NULL, i = NULL) {
        setline(r, i)
    }
    
    # get a row (default) or a column of the matrix
    getline <- function(i = NULL, row = TRUE) {
        if(is.null(i)) 
            return(message("index can not be null"))
        r <- NULL    
        if(row){
            if(i > nrow(x))
                return (message("index out of bounds"))
            r <- x[i,]
        }
        else {
            if(i > ncol(x))
                return (message("index out of bounds"))
                
            r <- x[, i]
        }
        r
    }
    
    #get a column of the matrix
    getcol <- function(i = NULL) {
        getline(i, FALSE)
    }
    
    #get a row of the matrix
    getrow <- function(i = NULL) {
        getline(i)
    }
    
    get <- function() x
    setsolve <- function(x1) x_1 <<- x1
    getsolve <- function() x_1
    
    list(set = set, get = get, set_vij = set_vij,
        setsolve = setsolve, 
        getsolve = getsolve,
        setcol = setcol, 
        setrow = setrow,
        setline = setline,
        getline = getline,
        getcol = getcol,
        getrow = getrow)
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
