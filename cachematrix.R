## Put comments here that give an overall description of what your
## functions do

## makecacheMatrix
## Input: a square matrix that is invertible
## Output: a list of functions that do the following:
    ## "sets" the value of the matrix
    ##  "gets" the value of the matrix
    ##  " sets inverse" of the value of the matrix
    ## "gets inverse" of the value of the matrix


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)        

}


## Write a short comment describing this function
## The function calculates the inverse of a matrix given a matrix ## as argument
## it first checks to see if theinverse has already been ## calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the ## inverse of the matrix and sets the value of the inverse in the ##cache via the 'setinverse' function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    my_inverse <- x$getinverse()
            if(!is.null(my_inverse)) {
                    message("getting cached data")
                    return(my_inverse)
            }
            data <- x$get()
            my_inverse <- solve(data, ...)
            x$setinverse(my_inverse)
            my_inverse
}
