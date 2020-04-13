## makeCacheMatrix takes argument *x* and creates a cached object with 
## *x*. This object can store the matrix's inverse as well to save time on
## intensive matrix function calculations.
## Example: M1 <- makeCacheMatrix(matrix(c(-3, 5, 1, 0), 2, 2))
## To get matrix once set: M1$get()
## To set new matrix: M1$set(matrix(c(2, 0, 0, 5, 3, 6, 1, 5, 7), 3, 3))
## To get inverse once calculated using cacheSolve: M1$GetInv()

makeCacheMatrix <- function(x = matrix()) {
        M_inv <- NULL
        set <- function(y) {
                x <<- y
                M_inv <<- NULL
        }
        get <- function() x
        SetInv <- function(inverse) M_inv <<- inverse
        GetInv <- function() M_inv
        list(set = set, get = get, SetInv = SetInv, GetInv = GetInv)
}

## cacheSolve finds the inverse of a cached matrix object created using
## *makeCacheMatrix.* It first checks to see if the inverse has already been
## calculated, and if so, returns the cached inverse. If not, it calculates
## the inverse and caches it for future access.
## Example: M1 <- makeCacheMatrix(matrix(c(-3, 5, 1, 0), 2, 2))
##      cacheSolve(M1)

cacheSolve <- function(x, ...) {
        M_inv <- x$GetInv()
        if(!is.null(M_inv)) {
                message("getting cached data")
                return(M_inv)
        }
        Mat <- x$get()
        M_inv <- solve(Mat, ...)
        x$SetInv(M_inv)
        M_inv
}
