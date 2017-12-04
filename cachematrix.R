## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversion
## 4. get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversion <- function(inversion) m <<- inversion
    getinversion <- function() m
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}


## The following function calculates the inversion of the special "matrix"
## created with the above function. However, it first checks to see if the
## inversion has already been calculated. If so, it `get` s the inversion from the
## cache and skips the computation. Otherwise, it calculates the inversion of
## the data and sets the value of the inversion in the cache via the `setsolve`
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinversion()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversion(m)
    m
}

