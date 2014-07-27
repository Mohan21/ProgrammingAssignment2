## The first function, makeCacheMatrix creates a matrix, which is really 
## a list containing a function to
##
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matix
##

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL

        set <- function(y) {

                x <<- y
                m <<- NULL
        }
        get <- function() x

        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## The following function calculates the inverse of the 
## matrix created with the above function. However, it 
## first checks to see if the inverse has already been calculated. 
## If so, it gets the inverted matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {

	m <- x$getinverse()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()

        m <- solve(data, ...)

        x$setinverse(m)

        m
        ## Return a matrix that is the inverse of 'x'
}












