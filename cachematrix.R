## Solution to the 2nd programming assignment
## of the R Programming course

## Create a CACHED matrix from the input matrix
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL   #Cached value

    set <- function(m) {    #Initialize the cache
        x <<- m
        inverseMatrix <<- NULL
    }

    get <- function() x

    setInverse <- function(inverse) inverseMatrix <<- inverse

    getInverse <- function() inverseMatrix

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculates the inverse of 'x', either by getting any value
## previously calculated or calculating it and saving a cached copy
## for later use.
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i <- x$getInverse() # Get the inverse from the cache
        if (!is.null(i)) {  # HIT. Return the precalculated value
            message("getting cached data")
            return(i)
        }
        # MISS. Calculate the inverse and store inside cache
        tmp <- x$get()
        i <- solve(tmp, ...)
        x$setInverse(i)
        i
}
