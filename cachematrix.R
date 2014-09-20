
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {                                 ##Set the value of the Matrix
        x <<- y
        inv <<- NULL
    } 
    get <- function() x                                  ##Get the value of the Matrix  
    setinv <- function(solve) inv <<- solve              ##Set the value of the Inverse   
    getinv <- function() inv                             ##Get the value of the Inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}                                                       ##The list of the "special" Matrix  

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated, then the cachesolve  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                              ##Check if the inverse matrix has already been calculated
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)                                ##Return the inverse already calculated
    }
    data <- x$get()
    inv <- solve(data, ...)                        ##Calculate the inverse of the Matrix
    x$setinv(inv)                                  ##Set the value of the Inverse   
    inv
        
}
