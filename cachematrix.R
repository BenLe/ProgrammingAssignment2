## Caching an inverse of a square invertible matrix. An input is assumed to be a square invertible matrix


## This function creates a special "matrix" object that can cache its inverse
## This function returns a list of functions (set, get, setinverse, and getinverse)

makeCacheMatrix <- function(x = matrix()) {
        
        #initialise a local variable to contain an inverted matrix
        invm<- NULL
        
        #function to set the current matrix input to a new one
        set<- function(y=matrix()){
                x<<-y
                invm<<-NULL
        }
        
        #function to return the input matrix
        get<- function() x
        
        #function to store an inverted matrix calculated by CacheSolve function below in invm
        setinverse <- function(inverse) invm<<-inverse
        
        #function to return an inverted matrix stored in invm
        getinverse<- function() invm
        
        #retrun a list of functions created above
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        
        #initialise a local variable by assigning it to a inverted matrix stored in function makeCacheMatrix
        invm<-x$getinverse()
        
        #if the inverted matrix variable invm declared above is not empty, return the result
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        
        #get the input matrix from get() created in makeCacheMatrix
        data<-x$get()
        
        #compute an inverse of the input matrix using solve()
        invm<-solve(data)
        
        #transfer the result to variable invm declared within function makeCacheMatrix
        x$setinverse(invm)
        #return computed inverse of input matrix
        invm
}
