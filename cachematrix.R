# makeCacheMatrix initialises m (which is set to NULL) and x.
# The set function then assigns NULL to m and y to x in the parent environment.
# Then, the get function ('getter') retrieves x from the parent environment.
# The 'setter' setinverse() is assigned the value of the inverse (solve) to m in the parent environment.
# The last step is defining the getter for m (using lexical scoping to find m).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve returns m from cache if it is not NULL (which it is set initially in makeCacheMatrix).
# If m is NULL, i.e. m has not been calculated and saved in the cache, the function sets m to the inverse using
# the solve function and stores o the value obtained as m for future operations using lexical scoping.

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
    
}




