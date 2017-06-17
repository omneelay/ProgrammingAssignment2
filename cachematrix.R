## These functions calculate the inverse of a matrix (using the solve function). They do this by first creating a special matrix and setting the inverse in the first function. The second function will look through the cache (a different environment) to see if the inverse has already been calculated. If it has been calucated, it gets the inverse from the cache. If it has not been calculated, the function will calculate the inverse of the matrix and then store the inverse in the cache.

## This function creates a matrix by first setting the value of the matrix, then getting the value of the matrix, then setting the inverse of the matrix, and finally getting the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## This function calculates the inverse of the special matrix that was made by the makeCacheMatrix function only if the inverse has not already been calcualted and stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached data")
            return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
