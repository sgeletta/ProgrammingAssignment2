## The following two functions create or store matrix values,
## calculates the inverse and cache the values of the inverse
## It is assumed that always inversible matrix is provided  to the function
## (i.e., there is no exception handling to check if the matrix is not inversible.

## The first function does the following: (a) set the values of the matrix, 
## (b) get the value of the matrix, (c) set the value of the inverse and (d) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmat <- function(solve) m <<- solve
        getmat <- function() m
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)

}


## The following function inverts the matrix that is set up by the above function.
## However, it first checks if the inverse has already been calculated. If the inverse
## already exists, then it returns the cached values.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmat(m)
        m

}
