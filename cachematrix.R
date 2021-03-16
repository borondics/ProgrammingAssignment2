## Caching matrix inverse, which can take a long time to compute

## makeCacheMatrix will create a special matrix that has get and set methods 
#  as well as can set and get its own inverse. I think it is like an object in 
#  Python.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(solve) inv <<- solve
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve will compute the inverse of a matrix that has been created with
#  makeCacheMatrix. If the inverse already exists, then it will simply get it 
#  from the cache, so no actual computation will be done.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message('getting the cached inverse matrix')
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setinv(inv)
    
    inv
}