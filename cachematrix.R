## This code should help to avoid redundant heavy computational costs of matrix
## .. inversion .. The fisrt function prompts the user to input the matrix and can work
## .. in parallel with second function (calculation of the matrix inversion).
## Please note that the matrix has to be square and non-singular. 
## Non-singular matrix should have a non zero determinate. Check with det().
## If the matrix inverse has not changed from previuos attempts, the output will 
## .. notify the user that a cashed result is being reported.
## Please see the examples after the code hereafter.
## Try it out and have fun guys :)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x<<-y
        inv<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv<<- inverse
    getinverse <- function() inv
    list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinverse()
    if(!is.null(inv)){
        message("Getting Cashed Data")
        return(inv)
    }
    matrix <- x$get()
    inv<-solve(matrix)
    x$setinverse(inv)
    inv
}

## Examples
## Try these out!
## new_matrix <- makeCacheMatrix(matrix(1:16, 4, 4)) ... Has to be a squre matrix 
## new_matrix$get() ... This will get the values of the matrix and store it
## new_matrix$getinverse() This will return Null because not stored inverse yet
## cashsolve(new_matrix) ... This will return matrix inverse and cash it
## cashsolve(new_matrix) ... This will return the same value with a message notifying the 
## ... user that a cashed value is being returned
## new_matrix$set(matrix(1:9,3,3)) ... Set a new value for the matrix and deletes
## ... the cashed value of the cashed matrix inverse
## new_matrix$getinverse()... will return Null as the stored (cashed) matrix inverse is deleted 
## cashsolve(new_matrix) ... new values for the inverse
## have fun guys :)