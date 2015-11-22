## The purpose of this programm is to write a square matrix to a cache and 
## solve the inverse of the matrix, or retrieve the cache if this has already
## been done. 


##The function "makeCacheMatrix" contains the following subroutines:
#        set: writes the matrix
#        get: retrieves the written matrix
#        setInverseMatrix: write the inverse (solve) of the matrix
#        getInverseMatrix: retrieves the value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) m <<- solve
        getInverseMatrix <- function() m
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
} 


## Write a short comment describing this function

# Solve the inverse of the matrix, or retrieve it if it from the cache
# was already solved (if the cache 'm' is not null).

cacheSolve <- function(x, ...) {
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverseMatrix(m)
        m
}
