## This program will take advantage of the scoping rules of the R language.
## It cached the inverse of a matrix (potentially time-consuming computation)
## rather than compute it repeatedly.

## The function makeCacheMatrix creates a special 'matrix'  object that can
## cache its inverse. 
makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {      #setting matrix
                x <<- y
                inv <<- NULL      #re-initialize inverse in the parent environment
        }
        
        get <- function() x       #returning matrix entered
        setinverse <- function(solve) inv <<- solve     #setting inverse matrix
        getinverse <- function() inv         #returning cached inverse matrix
        
        list(set = set, get =get,            #setting list of functions
             setinverse = setinverse, 
             getinverse = getinverse)       
        
}

##The function cacheSolve taking matrix in makeCacheMatrix and make it inverse. 
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()     #if inverse exist before, write a message
        if(!is.null(inv)) {
                message("getting cached inverse data")
                return(inv)       #retrieving the inverse
        }
        data <- x$get()           #get the inverse
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}