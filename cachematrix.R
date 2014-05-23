## makeCacheMatrix caches a matrix 
## cacheSolve returns the inverse of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(matrix) m <<- matrix
    getInverseMatrix <- function() m
    list(set = set, get = get, getInverseMatrix = getInverseMatrix, setInverseMatrix = setInverseMatrix)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverseMatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverseMatrix(m)
    m
}
