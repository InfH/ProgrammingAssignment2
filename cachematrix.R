## These functions construct a matrix-like object that caches its inverse upon first computation. If the inverse is calculated afterwards, the function will retrieve the value stored in cache first.


## makeCacheMatrix creates the cacheMatrix type. It is a matrix, with additional parameters to set the value, get the value, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## cacheSolve gets the inverse of the input cacheMatrix if it exists. If it doesn't, it calculates the inverse and stores it in the appropriate slot of the cacheMatrix object

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinv(i)
    i
}
