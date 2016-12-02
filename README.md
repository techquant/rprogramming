# rprogramming
##creates a special “matrix” object that can cache its inverse
## computes the inverse of the “matrix” returned 

## creates a  special matrix and returns a list of functions that
## 1.set the matrix
## 2.get the matrix
## 3.set the inveserse
## 4.get the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initial assignment
    i <- NULL
    set <- function(y){
    ## Assign the input argument & NULL to the x and i object in the parent environment
        x <<- y    
        i <<- NULL 
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
   
}


## computes the inverse of the matrix returned
## if the inverse has been calculated, and matrix not changed, 
## it will retrieve the inveserse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'    
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Otherwise, calculate & return if cache is empty
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
