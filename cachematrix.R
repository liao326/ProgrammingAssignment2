## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# treat input x as matrix and solved value "i" as null

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {     
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setInverse <- function(inverse) i <<-inverse
        getInverse <- function() i
        list(set = set
             get = get,
             setInverse = setInverse
             getInverse = getInverse)
}         

## Write a short comment describing this function
#same here, changed "mean" to "solve" and "m" to "i"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("get inversed matrix")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setInverse(i)
        i
}
