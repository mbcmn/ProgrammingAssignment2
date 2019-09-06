###and whether the comments you include in your functions indicate that you understand
###lexical scoping and the superassignment operator (<<-) 

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

### add comments for each function to explain understanding

## whether the comments you include in your functions indicate that you understand
## lexical scoping and the superassignment operator (<<-) as used in these functions

### Commands
###
### a <- makeCacheMatrix(matrix(1:4, 2, 2))
### cachemean(a)


