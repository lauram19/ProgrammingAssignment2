## The functions listed below are used to create an object that stores a matrix
## and caches its inverse.

## The first function, makeCacheMatrix, creates a matrix, which is really a list which:
## 1.sets the value of the matrix
## 2.gets the value of the matrix
## 3.sets the value of the inverse
## 4.gets the value of the inverse

makeCacheMatrix <-function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setInverse<-function(inverse) inv<<-inverse
    getInverse<-function() inv
    list(set=set,get=get, setInverse=setInverse,getInverse=getInverse)
}



## The second function below computes the inverse of the matrix returned by the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
    inv<-x$getInverse()
    if(!is.null(inv)) {
        message("getting chached data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setInverse(inv)
    inv
}
