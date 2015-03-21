## Functions provide a possibility of getting inverses of matrixes
## and caching inverses got for matrices to reduce cost of inverse computation.

## function gets a matrix and creates an object that can store this matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set<-function(y){
        x<<-y
        invMatr<<-NULL
    }
    get<-function() x
    setInverted<-function(inv) invMatrix<<-inv
    getInverted<-function() invMatrix
    
    list(set=set, get=get,
         setInverted=setInverted,getInverted=getInverted)

}


## function gets a special "matrix" created by makeCacheMatrix and returns its inverse
## from cache if it has been already calculated. If there is no inverse in cache then 
## the function computes an inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix<-x$getInverted()
    if(!is.null(invMatrix)){
        message("getting cached inverted matrix")
        return(invMatrix)
    }
    data<-x$get()
    invMatrix<-solve(data,...)
    x$setInverted(invMatrix)
    invMatrix
}
