##This function creates a special "matrix" object 
##that can cache its inverse.

#contains 4 functions: set, get, setmean, getmean.
makeCacheMatrix <- function(x = matrix()) {         

    m <- NULL
    
    #set is a function that changes the matrix stored in the main function.
    #We don't need to use this function unless we want to change 
    #the matrix. "x <<- y" substitutes the matrix x with y (the input) 
    #in the main function (makeCacheMatrix). If it was "x <- y" it would 
    #have substitute the matrix x with y only in the set function. 
    #"m <<- NULL" restores to null the value of the inverse m, 
    #because the old inverse of the old matrix is not needed anymore. 
    #The new inverse needs to be recalculated through 
    #the function cacheSolve.
    set <- function(y) {  
            x <<- y
        m <<- NULL
    }
    
    #get is a function that returns the matrix x stored in the main 
    #function. Doesn't require any input.
    get <- function() x                             
    
    #setinverse and getinverse are functions very similar to set and get. 
    #They don't calculate the inverse, they simply store the value of 
    #the input in a variable m into the main 
    #function makeCacheMatrix (setinverse) and return it (getinverse).
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    #To store the 4 functions in the function makeCacheMatrix, 
    #we need the function list(), so that when we assign 
    #makeCacheMatrix to an object, the object has all the 4 functions.
    list(set = set, get = get,
         setinverse = setinverse,
         getsolveinverse = getinverse)
}



##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    #The first thing cachesolve does is to verify the value m, 
    #stored previously with getinverse, exists and is not NULL. 
    #If it exists in memory, it simply returns a message 
    #and the value m, that is supposed to be the inverse, 
    #but not necessarily.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #If it was the case, "return(m)" would have ended the function. 
    #So everything that follows this if() is a sort of else {}. 
    #data gets the vector stored with makeCacheMatrix, 
    #m calculates the inverse of the vector and 
    #x$setinverse(m) stores it in the object generated assigned 
    #with makeCacheMatrix.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

#Define a square Matrix A 
#To run the matrix through the code define an object,eg Mat <-makeCacheMatrix(A)
#Then cacheSolve(Mat)
# If you repeat the last line of code you will get the "getting cached data" message
