## These two functions are capable of calculating the inverse of a given matrix. Furthermore, the main 
## characteristic of these functions is that if the matrix on which the inverse is calculated has not 
## changed, the second function does not repeat the calculation, but simply collects the previously 
## calculated data that is stored in the environment of the functions

## makeCacheMatrix is able to do four things:
##      1. set: fix the matrix entered as variable x. Before that the empty variable inv is also crated
##      2. get: returns the matrix entered as variable x
##      3. setinverse: alowws to set up the inverse matrix. Ths will be used after for the second variable
##                     when calculate the inverse variable
##      4. getinverse: returns the setinverse value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y         # The <<- operator assign the x and inv variable to the father 
                inv <<- NULL    # enviroment of the function (y), that is to say to the enviroment
        }                       # of the function makeCacheMatrix
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve calculates the inverse of the matrix using the "solver" function of R. However, before 
## doing so, cacheSolve looks within the environment of the function itself to see if such a 
## calculation has already been made. In that case it returns the value of the inverse with the 
## message: "getting cached data" (this will happen when the inverse calculation is repeated on the 
## same original matrix). At the end the function return the matrix that is the inverse of "x"

cacheSolve <- function(x, ...) {  
                inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv 
}

## R is able to save the inverse matrix so you don't have to repeat the calculation thanks to 
## its "Scoping Rules" feature. 
