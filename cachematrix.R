#The makeCacheMatrix() and cacheSolve() work together to cache 
#the inverse of a matrix. When the same matrix is passed 
#next time, these functions will retrieve the value of last calculation 
#without doing it all over again. These functions took advantage of 
#lexical scoping and can save time. 

#MakeCacheMatrix() function takes a matrix as argument, and generates 
#a list of four functions (get, set, getinv and setinv) to get or set the values
# of the matrix and its inverse. This function also set the 
#initial matrix and its inverse as null. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(a){
        x <<- a
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inver) inv <<- inver
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#cacheSolve() function takes the output of makeCacheMatrix()
#as argument, runs getinv to evaluate if the inverse is cached. If not, 
#it will run get() and calculate the inverse, then run setinv() to 
#set the new inverse and return it. 

cacheSolve <- function(y, ...) {
    inv <- y$getinv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- y$get()
    inver <- solve(data)
    y$setinv(inver)
    inver
}

