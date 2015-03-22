## Overal function : Improving computation of matrix inversion by caching
## This way, when calculating the inverse of "previously cached matrix",
## there is not need to compute, but to just take the value from cache.

## makeCacheMatrix method will cache the input matrix
## caching is done via using the <<- operator to assign variable within that environment
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    get <- function() x
    set <- function(y) {
        x <- y 
        m <<- NULL
    }
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## caccheSolve method will check whether the inverse matrix is in the cache
## if so, just return the cache 
## else, solve() matrix to calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
    m <- x$getInverse()

    if(!is.null(m)) {
        return(m)
    }
    
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
