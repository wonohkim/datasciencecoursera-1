## R Programming Coursera
## Programing Assignment 2

#Function to compute and cache matrix inversion

#Step 1: Set the cache matrix and values
makeCacheMatrix <- function(x = matrix()) {#x is the supplied matrix    
    
    #Reset existing variables by setting the matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #Command to get the values of the matrix
    get <- function() x
    #Command to set the inverted matrix
    setinv <- function(inv) m <<- inv
    #Command to get the inverted matrix
    getinv <- function() m
    #Store the values
    list(set = set, get = get,setinv = setinv,getinv = getinv)
}

# Step 2: Compute or call inverted matix 

cacheSolve <- function(x, ...) { 
   
    #Get the stored value of inverted matrix
    m <- x$getinv()
    #If the value is not null (old matrix) get previous computation
    if(!is.null(m)) {
        message("getting cached data")
        return(m) #Print value and leave function
    }
    
    #else get matrix into data
    data <- x$get()
    #Compute inverted matrix from data
    m <- solve(data, ...)
    #Set inverted to store
    x$setinv(m)
    #Print value
    m
}