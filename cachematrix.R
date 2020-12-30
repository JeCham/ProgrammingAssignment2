## This file create a special matrix object named makeCacheMatrix that
## allows caching of its inverse for repeated use within the environment


## Create special matrix objects that allows caching of its inverse
## inverse can be called or set using $getinv and $setinv functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function(y) {
                x
        }
        setinv <- function(inverse) {
                i <<- inverse
        }
        getinv <- function() {
                i
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Get the inverse of the special makeCacheMatrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        ## if cache exist, return cached inverse                
        cached_inverse <- x$getinv()
        if(!is.null(cached_inverse)) {
                message("getting cached inverse")
                return(cached_inverse)
        }
        
        ## otherwise, compute the inverse and return it
        matrix <- x$get()
        compute_inverse <- solve(matrix)
        x$setinv(compute_inverse)
        compute_inverse
}
