## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{  
    
        ## z is initialized to NULL to store the inverse of the matrix x
        z<- NULL
    
        ## This caches the computed inverse in the z variable.
        set <- function(y) 
    {
        x <<- y
        z <<- NULL  # Reset the inverse when the matrix is changed
    }
    
        ## Return the matrix x
        get <- function() x
    
        ## Set the inverse of the matrix
        setInverse <- function(inverse) z <<- inverse
        
        
        ## Method to get the inverse of the matrix
        getInverse <- function() 
        return(z)
    
        list(set = set, get = get, 
            setInverse = setInverse, 
            getInverse = getInverse)
}



## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## Retrieve the cached inverse
        z <- x$getInverse()
        
        ## Return the inverse if its already set
        if (!is.null(z)) 
    {
        message("getting cached data")
        return(z)
    }
    
        ## Get the matrix from our object
        mat <- x$get()
        
        ## Compute the inverse
        z <- solve(mat, ...)
        
        ## Cache the inverse
        x$setInverse(z)
    
        return(z)
}


