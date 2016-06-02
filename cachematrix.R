## This pair of functions takes a matrix and finds its inverse. To save
## processing time, if the same matrix has already been checked, it uses a 
## cached version of the inverse matrix.

## The "makeCacheMatrix" function caches the inverse of the source matrix for future use

makeCacheMatrix <- function(x = matrix()) {
        #Start by creating a NULL matrix for the inverse
        inversematrix <- NULL
        
        # Create the function for caching
        set <- function(y = matrix) {
                x <<- y
                inversematrix <<- NULL
        }
        
        # Get the value of the inverse matrix
        get <- function() x
        
        # Calculates inverse of the original matrix
        setInverse <- function(solve) inversematrix <<- solve
        
        # Gets the inverse matrix
        getInverse <- function() inversematrix
        
        # Puts all the values for makeCacheMatrix into list called "list"
        list( 
                set = set, 
                get = get, 
                setInverse = setInverse, 
                getInverse = getInverse
        )
}


# The "cacheSolve" function gets the cache of the matrix, if it exists. If it doesn't 
# exist, the function creates a new inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getInverse()
        
        # Is there a cached version? If so, use it
        if(!is.null(inversematrix)) {
                message("getting cached data for the inverse of the matrix")
                return(inversematrix)
        }
        
        # if no cached version, calculate it and use it
        message("no cache available; getting a new inverse matrix")
        data <- x$get()                               
        inversematrix <- solve(data, ...)
        x$setInverse(inversematrix)
        inversematrix
}

