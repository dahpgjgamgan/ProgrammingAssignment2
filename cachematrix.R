## Objects for caching matrix inverse operations
## functions do

## Creates a wrapper object around a matrix, with following operations:
## get - retrieves the underlying matrix
## set - sets the underlying matrix
## get_inverse - retrieves the matrix inverse 
## set_inverse - sets the matrix inverse

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(value) {
            m <<- value
            inv <<- NULL
        }
        get <- function() m
        set_inverse <- function(inverse) inv <<- inverse
        get_inverse<- function() inv
        list(
            set = set,
            get = get,
            set_inverse = set_inverse,
            get_inverse = get_inverse
        )
}


## Calculates matrix inverse of a given matrix object (created by makeCacheMatrix)
## stores the result in wrapper object and returns it

cacheSolve <- function(matrix_wrapper, ...) {
        #try to retrieve cached inverse
        inv = matrix_wrapper$get_inverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        # cache not present, need to compute
        underlying_matrix <- matrix_wrapper$get()
        inv <- solve(underlying_matrix, ...)
        matrix_wrapper$set_inverse(inv)
        inv
}
