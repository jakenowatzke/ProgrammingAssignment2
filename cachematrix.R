
## ===Write a short comment describing this function===
###makeCacheMatrix is a function which sets and gets the value
###of both the matrix and the inverse matrix
####"This function creates a special "matrix" object that can cache its inverse."
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}
## ===Write a short comment describing this function===
###  cacheSolve is a function which calculates the inverse matrix defined in the above
###function, but first checks to see if it has been cached, in which case it will not 
###need to calculate anything further
####"This function computes the inverse of the special "matrix" 
#### returned by makeCacheMatrix above."

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

#Example of working function below
MatA <- matrix(data = c(3,-7,5,2), nrow = 2, ncol = 2)
MatCache <- makeCacheMatrix(MatA)
cacheSolve(MatCache)
