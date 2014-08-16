## This version of makeCacheMatrix is **ONLY** intended to work on square, 
## invertible matrices.
##
## makeCacheMatrix is a function that receives a matrix x as its argument and
## returns a list of functions (set(), get(), setinverse(), getinverse()).
##
## set() sets the value of the matrix.
## get() gets the value of the matrix.
## setinverse() sets or stores the value of the matrix's inverse.
## getinverse() gets the value of the matrix's inverse.
##
## Use makeCacheMatrix() along with cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
        ## inv is a variable to store inverse of matrix x.
        inv <- NULL     ## Ensure local instance 'inv' is initialized to NULL.
        
        set <- function(y) {  
                x <<- y ## Set value of matrix x beyond the current environment. 
                
                ## Ensure value of 'inv' outside the current environment
                ## of set()'s definition is reset to NULL, since we have
                ## a new matrix. This is necessary if set() is called
                ## directly.
                inv <<- NULL      
        }
        
        get <- function() x   ## Establish that get() contains the value of x().
        
        ## Establish setinverse() as equivalent to calling R
        ## standard base function solve(). Ensure value of 'inv'
        ## outside the current environment is now set to result 
        ## of calling solve().
        setinverse <- function(solve) inv <<- solve   
        
        ## Establish that getinverse() contains value of inv, 
        ## which is the inverse of x().
        getinverse <- function() inv  
        
        ## Return a list where list items are respectively named
        ## the same as the functions() defined/initialized 
        ## within makeCacheMatrix().
        list(set = set, get = get,    
             setinverse = setinverse, 
             getinverse = getinverse) 
}

## cacheSolve() checks to see if the inverse to a matrix is alredy stored.
## If the matrix index already exists, cacheSolve() returns the inverse
## from the existing cache. If it does not already exist, it creates it and 
## stores it for reuse.

cacheSolve <- function(x = matrix(), ...) {
        ## Check to see if the inverse of matrix in formal argument to
        ## cacheSolve() is already stored.
        inv <- x$getinverse()
        
        ## If inverse of desired matrix already available, provide it 
        ## rather than recalculate it.
        if(!is.null(inv)) {     
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()         ## Store a copy of current matrix in data.
        inv <- solve(data, ...) ## Store the inverse of data in inv.
        x$setinverse(inv)       ## Store the inverted matrix in list returned
        ## by makeCacheMatrix().
        inv
}