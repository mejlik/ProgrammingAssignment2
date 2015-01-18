## In order to return an inverse of a matrix, function check if it was calculated before.
## If so then return it, if not - calculate it and save it (cache it)

## A way of saving a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL #setting value of an inverse to null
    
    #setting a new matrix and a value of its inverse to null
    set <- function(y) { 
        x <<- y
        m_inv <<- NULL
    }
    
    #returning a matrix
    get <- function() x
    
    #setting value of an inverse
    setinverse <- function(inverse) m_inv <<- inverse 
    
    #returning an inverse
    getinverse <- function() m_inv
    
    #returning a list of elements
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## returning an inverse of matrix. 
## If it was calculated before getting it from cache.
## If it wasn't calculated then calculate it and save.

cacheSolve <- function(x, ...) {
    #checking if an inverse of matrix is already calculated
    m_inv <- x$getinverse() 
    if(!is.null(m_inv)) { 
        #if yes - returning it
        message("getting cached data")
        return(m_inv) 
    }
    #if not - calculate it
    data <- x$get() 
    m_inv <- solve(data, ...)
    #save it (cache)
    x$setinverse(m_inv)
    #and return
    m_inv
}
