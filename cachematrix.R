## set- sets input argument to x and NULL to m in parent enviro
## get- retrieves x from parent function
## setsolve- uses <<- to assign input argument to m in parent enviro
## getsolve- finds correct symbol m to retrive its value
## list- assigns each function as an element on list and returns to parent enviro so that each element can be called by name later using $

## Here I created the object "makeCacheMatrix" that returns a fully formed object to be used in "cacheSolve" code
## x is initialized as a matrix function argument and
## m is initialized as an object to be used by later code in fuction

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set<- function (y){
                x<<- y
                m<<- NULL
}
        get<- function() x 
        setsolve<- function(solve) m<<- solve
        getsolve<- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## "cacheSolve" retrieves inverse matrix from object passed as an argument
## calls getsolve on input object
## checks to see whether result is NULL
## if !is.null = FALSE, R will get matrix from input, inverse it, use setsolve to set inverse in input and return value to parent enviro

cacheSolve <- function(x, ...) {
        m<- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<- x$get()
        m<- solve(matrix, ...)
        x$setsolve(m)
        m
        ## Returns a matrix that is the inverse of 'x'
}
