
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## creating a special matrix function that can cache its inverse
        
        makeCacheMatrix <- function(x = matrix()) {
                inv<-NULL       ##initialization of inverse as null
                set<-function(y){
                        x<<-y
                        inv<<-NULL
                        
                }
                get<-function(){x}    ##function to get matrix say x
                setInverse<-function(inverse){
                        inv<<-inverse
                }
                getInverse<-function(){inv} ##function to get its inverse
                list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
        }
        
        
        ## Write a short comment describing this function
        ## to get cache data
        
cacheSolve <- function(x, ...) {
                ## Return a matrix that is the inverse of 'x'
        }
        inv<-x$getInverse()    ##checking whether inverse is null
        if (!is.null(inv)) {
                message("getting the cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)   ## calculating inverse value
        x$setInverse(inv)
        inv 
        ## Return a matrix that is the inverse of 'x'
}
