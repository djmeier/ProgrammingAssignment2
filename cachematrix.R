## These functions will compute the inverse of a matrix,
## allowing for cached data to be used if present.
## This saves on processing after the first execution
## with a particular defined matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## initialize m/set to NULL
        set <- function(y) {
                x <<- y ## Set value of "x" using "superassignment" operator
                m <<- NULL ## Reset value of "m" to NULL using "superassignment" operator
        }
        get <- function() x ## define "get" function, simply returns "x"
        setinverse <- function(z) m <<- z ## Set value of "global" "m" using "superassignment" operator
        getinverse <- function() m  ## getinverse function returns "global" "m" var
        
        return(list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse))
}



## This function computes the inverse of the special "matrix"(x) returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting cached data...")
                return(m)
        }
        data <- x$get() ## get the matrix
        m <- solve(data, ...) ## execute "solve" function to apply inverse
        x$setinverse(m) ## apply results back
        return(m)
}
