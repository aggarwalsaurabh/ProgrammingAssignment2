## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Functions in below file cahce the copy of inverse of a matrix so that if repetative operations needed then cached version can be used to save CPU

## This function creates a special matrix which is a list of following 
## Set the value of matrix
## Get the value of matrix
## Set the value of inverse
## Get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)	##Prepare a list
}


## This function determine Inverse of a matrix. If matrix inverse is already present then it fetch it from cache else perform fresh operations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
