## Put comments here that give an overall description of what your
## functions do
## Overall, these two functions allow you to cache the inverse of a matrix

## Write a short comment describing this function
## This function returns a list of functions 
## with the matrix x to be inverted (default empty matrix)
## so that we can call these functions on x
makeCacheMatrix <- function(x = matrix()) {
        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function() x
        setCachedSolved <- function(solved) m <<- solved
        getCachedSolved <- function() m
        list(set = set, get = get,
             setCachedSolved = setCachedSolved,
             getCachedSolved = getCachedSolved)
        
}


## Write a short comment describing this function
## take a makeCacheMatrix with matrix passed to it to be inversed
## if you haven't already got the inverse, compute it
## otherwise, return the 'cached' one

## How I spent a long time trying to test this:
## You can test this function using these commands
## set up an invertible matrix
## c=rbind(c(1, -1/4), c(-1/4, 1))
## call the makeCacheMatrix with this 
## aa <- makeCacheMatrix(c)
## call cacheSolve on this to return the inverse of aa
## cacheSolve(aa) 
## now call it again so you can see the cached one
## cacheSolve(aa)
## now set the inverse matrix to a variable
## z <- cacheSolve(aa)
## bb <- makeCacheMatrix(z)
## and get your original matrix back
## cacheSolve(bb)

## This helped for me to work out how to test it - https://class.coursera.org/rprog-003/forum/thread?thread_id=664 
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        empty <- matrix()
        m <- x$getCachedSolved()
        isEmpty <- identical(empty,m)
        if (!isEmpty) {
                message("getting cached data")
                return(m)   
        }
        data <- x$get()
        m <- solve(data)
        x$setCachedSolved(m)
        m        
}


