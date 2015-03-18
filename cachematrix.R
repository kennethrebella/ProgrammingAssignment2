
## This function essentially creates a list of functions. 
## When we passing the functions within a function creates an environment 
## where set, get, setmatrix,and getmatrix are defined 
## it also caches variable m to the parent environment


makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## cacheSolve pulls up the values established when set is called
## it then takes that value and runs the solve function on it
## which returns the inverse of a square matrix. 
## It then sets the matrix and finally gives the inverted matrix 

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
