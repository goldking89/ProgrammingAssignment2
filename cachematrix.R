# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" created by makeCacheMatrix function above.

cacheSolve <- function(x, ...) {

        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        return(inv)
}

# Assume the matrix is invertible, use solve() function to compareresults
# set.seed(123)
# mat <- matrix(rnorm(9),3,3)
# temp <- makeCacheMatrix(mat)
# cacheSolve(temp)
