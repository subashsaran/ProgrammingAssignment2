## This program takes a matrix as input, computes its inverse and stores the inverse in cache. 
## When the inverse of the same matrix is required, the result is shown from the cache instead of computing it again.
##
##############################################Function:makeCacheMatrix#############################################################################
## The following function makeCacheMatrix will create a list or special "matrix" object  
## that can cache its inverse.The list object contains functions to set and get the matrix as well as its inverse.

## Function receive matrix as input into variable "x".

makeCacheMatrix <- function(x = matrix()) {        
        
        i <- NULL                   ## Initialize the inital inverse variable "i" to null.
        
        set <- function(y) {	    ## Set function, to receive a input matrix into variable "x" when a value passed 
                x <<- y             ## down from a calling function.And also to nullify the inverse variable "i".
                i <<- NULL          ## In both cases ''Superassignment" operator is used to create the variables in  global environment
        }                           ## when it never find an exising variable.But if there is an existing variable then it will be redefined.
        
        get <- function() x	    ## Get function, to just return the matrix variable "x"s present value.
        
        setinverse <- function(inverse) 
                i <<- inverse	    ## Set function, to receive a matrix inverse as input and super assigns to variable "i".
        
        getinverse <- function() i  ## Get function, to return the matrix variable "x"s present value.
        
        list(set = set, get = get,  ## Function return a list object to set and get the matrix as well as its inverse.
             setinverse = setinverse,
             getinverse = getinverse)
}
##
##############################################Function:cacheSolve##################################################################################
## The following function cacheSolve calculates the inverse of the special matrix object created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

## Function receive list or special matrix object as input into variable "x".

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()         ## Call the get inverse function from makeCacheMatrix and assign the value to variable "i".
        
        if(!is.null(i)) {           ## Make sure that above variable "i" got a value, if so return it with a message. 
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()             ## When variable "i" have not got any value it will call the get function and receives the input into 
                                    ## "data" variable.     
        i <- solve(data, ...)       ## Sent the "data" variable into solve function to compute the inverse of the input matrix.   
                                    ## The result kept in variable "i".
        x$setinverse(i)             ## Call the set inverse function from makeCacheMatrix by sending the above result ïn "i".
                                    ## Which will help the superassignment.           
        i                           ## Return a matrix that is the inverse of 'x'
}
##################################################################################################################################################