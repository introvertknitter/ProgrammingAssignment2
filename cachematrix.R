##  makeCacheMatrix  and  cacheSolve  are a pair of functions that cache the inverse of a matrix
##  it is assumed, that assume that the matrix supplied is always invertible

## makeCacheMatrix  creates a special "matrix" object that can cache its inverse, it uses argument x which is a invertible matrix

makeCacheMatrix <- function(x = matrix()) { 	
    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix - it uses the output of makeCacheMatrix, 

cacheSolve<- function(x, ...) {
        m <- x$getinverse ()
        if(!is.null(m)) {		##if inversion is not null...
                message("getting cached data")
                return(m)		##...then print inversed matrix
        }
        data <- x$get()			##get the vector with data
        m <- solve(data, ...)		##calculate inversion
        x$setinverse(m)			
        m				##print inversed matrix
	
}




