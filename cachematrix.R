## The first function "makeCacheMatrix" creates a special "matrix" object that  
## can cache its inverse. It also returns a list of functions 
## that allow you to get the matrix or the inverse and set the matrix or its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- matrix()
        set <- function(y) {
                x <<- y
                i <<- matrix()
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function gives you the inverse of the special matrix you
## had previously created. If the inverse has previously been calculated,
## it doesn't re-calculate it. It just gets the cached inverse.
## The argument for this function is the list created by the previous 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.na(i[1])) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
