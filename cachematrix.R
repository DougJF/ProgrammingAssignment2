## This pair of functions caches the inverse of a matrix rather
## than computing it repeatedly

## This one creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
    m <- NULL
    set <- function(y){
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


## This one computes the inverse of the special 'matrix' returned
## by makeCacheMatrix above. 

cacheSolve <- function(x, ...){
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrx <- x$get()
    m <- solve(matrx, ...)
    x$setinverse(m)
    m
}
