## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse matrix. 

 
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of the inverse matrix 
## 4. get the value of the inverse matrix  
 
makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL 
        set <- function(y) { 
                x <<- y 
                inv <<- NULL 
        } 
        get <- function() x 
        setinv <- function(inv_matrix) inv <<- inv_matrix 
        getinv <- function() inv 
        list(set = set, get = get, setinv = setinv, getinv = getinv) 
} 
 
 
 
 
## The following function calculates the inverse matrix of the matrix created with the function above. 
 
 
cacheSolve <- function(x, ...) { 
         ## Return a matrix that is the inverse of 'x' 
        inv <- x$getinv() ##get inverse matrix from cache.     
        
        ## Test if the the inverse matrix has been caculated. If it has, then call the inverse matrix from cache; if not, then calculate it and return it. 
        if (is.null(inv) { 
                new_m <- x$get() 
                inv_matrix <- solve(new_m) 
                x$setinv(inv_matrix) 
                return(inv) 
        } else { 
                message("getting cached data") 
                return(inv) 
        } 
} 
