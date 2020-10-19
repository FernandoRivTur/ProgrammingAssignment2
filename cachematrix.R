## This function creates a matrix and shows a list containing 
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(solve) {inv <<- solve}
        getinv <- function() {inv}
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function calculates the inverse of the matrix and if it has already 
##been calculated, it gives it to you without needing to calculate it again because it has been saved in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
