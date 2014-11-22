makeCacheMatrix <- function(mtx = matrix()) {
        
        # creates a list with four functions: 
        # save and get an invertible matrix from a local environment (cache)
        # save its inverse to or get it from a local environment (cache)
        
        inv <- NULL
        set <- function(y) {
                mtx <<- y
                inv <<- NULL
        }
        get <- function() mtx
        setinv <- function(z) inv <<- z
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(cachmtx, ...) {
        
        # returns the inverse of a matrix
        # checks first whether the inverse has already been calculated
        # it is assumed that the matrix supplied to the function is invertible
        
        # input cachmtx : a list object returned by the makeCacheMatrix function
        
        inv <- cachmtx$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- cachmtx$get()
        inv <- solve(data, ...)
        cachmtx$setinv(inv)
        inv
}