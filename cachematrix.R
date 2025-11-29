## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that stores a matrix and 
## caches its inverse. it set/get matrix and set/get the cached inverse of it

makeCacheMatrix <- function(x = matrix()) {
        i<- NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinverse<- function(inverse) i<<-inverse
        getinverse<- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of the matrix in makeCacheMatrix object.
## if the inverse has already been computed it retrieves it from cache instead
## of recalculating it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)){
                message("Getting cached data")
                return (i)
        }
        data<-x$get()
        i<- solve(data,...)
        x$setinverse(i)
        i
}
