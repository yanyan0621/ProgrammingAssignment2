## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL   #  m will store our 'inverse function' and it's reset to NULL every time makeCacheMatrix is called
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x     # this function returns the value of the original matrix
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


## he inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  # the input is an object created by makeVector
    m <- x$getinv()               # accesses the object 'x' and gets the inverse of matrix
    if(!is.null(m)) {              # if inverse matrix was already cached (not NULL) ...

        message("getting cached data")  # ... send this message to the console
        return(m)                       # ... and return the inverse matrix ... "return" ends 
                                        #   the function cacheSolve(), note
    }
    data <- x$get()        # we reach this code only if x$getinv() returned NULL
    m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
    x$setinv(m)           # store the calculated inverse matrix in x (see setinv() in makeCacheMatrix)
    m                      # return the inverse matrix to the code that called this function

        ## Return a matrix that is the inverse of 'x'
}
