
### Assignment 2, week 3 R programming course

# The first function, makeChacheMatrix creates a special "Matrix", which is really a
#list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## The following function calculates the mean of the special "matrix" created
## with the above function (makeCacheMatrix). However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse matrix in
## the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
  }


#To test if the functions worked
A <- matrix(c(1,2,3,4),2,2)
A1 <- makeCacheMatrix(B)
A2 <- cacheSolve(B1)

print(A2)




## this explains everything in the example in the beginning of the assignment
## https://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r

# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
#
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }
