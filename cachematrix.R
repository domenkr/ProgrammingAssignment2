## Put comments here that give an overall description of what your
## functions do

## This function creates a list of setters and getters for matrix and it's inverse. Specifically it creates get 
## function that returns the matrix, a set function that sets the matrix to y input parameter, a getinverse function 
## that returns the inverse of matrix and a setinverse function that sets inverse of a matrix to inverse input 
## parameter. Finally it returns all those functions in a list. It's kinda like a class in other languages where you 
## make variables private and only use getters and setters to access and/or modify them
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function() x
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        getinverse <- function() inv
        setinverse <- function(inverse) inv <<- inverse
        list(get=get, set=set, getinverse=getinverse,setinverse=setinverse)
        
}


## This function demands a list with all the set and get functions that correspond to matrix and its inverse.
## It then uses these functions to check whether matrix already has inverse computed (and returns it in that case)
## or it computes the inverse and then sets it via setinverse function for later use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}