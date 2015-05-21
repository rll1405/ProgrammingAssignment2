#makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#1. set the value of the vector
#2. get the value of the vector
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) { #1. set the value of the vector 
                x <<- y 
                s <<- NULL
        }
    get <- function() x #2. get the value of the vector
    setsolve <- function(solve) s <<- solve #3. set the value of the inverse
    getsolve <- function() s #4. get the the value of the inverse
    list(set = set, get = get, #return "vector" list of functions 1-4
         setsolve = setsolve, 
         getsolve = getsolve)
}


#cacheSolve calculates the inverse of the special "vector"
#created with makeCacheMatrix. It first checks if the
#inverse has already been calculated, and if so, skips the computation and gets 
#the inverse from the cache. If not, the inverse is calculated 
#and sets the value in the cache.

cacheSolve <- function(x, ...) {
     s <- x$getsolve()
        if(!is.null(s)) {   # if the inverse is already cached... 
        message("getting cached data")
        return(s)  # ...exit here.
    }
    data <- x$get()   # if not, assign the data to the "data" variable
    s <- solve(data, ...)   # compute the inverse
    x$setsolve(s) # call the function to cache the inverse
    s # return the inverse
}
