## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function below is creates a list containint ht eh following
##1. set the value of the vector
##2. get the value of the vector
##3. set the value of the mean
##4. get the value of the mean
## This is used by cachesolve for get, set, setinv, and getinv
makeCacheMatrix <- function(x = matrix()) {
      ##Iniatlise variables used this this
      InvM <- c()
      ## assign a value to an object in an environment that is 
      ##different from the current environment
      ##That is create a matrix in teh working enviorment
      set <- function(y) {
          x <<- y
          InvM <<- c()
        
      }
      
      ##get the value of hte matrix
      get <- function() x
      ##Invert the matric and store it in the cache
      setinv <- function(solve) InvM <<- solve
      ##get the value of the inverted matrix
      getinv <- function() InvM
      ##create a list to return the enviroment
      list(set=set, get=get, setinv=setinv, getinv=getinv )
      
}


## Write a short comment describing this function
## The function below returns the cached matrix m if it has m is not null, 
## and inverts a square matrix if the matrix is null. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', assign this to m
  m <- x$getinv()
  #if m already exists we don't need to do anything here, just simply return the cached verison
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  #if m does not exist here then go and get the data
  data <- x$get()
  #invert the matrix
  m <- solve(data, ...)
  #set the inversion 
  x$setinv(m)
  #return m
  m
  
}
## Function Test
## x = rbind(c(1, -1/3), c(-1/3, 1))
## m = makeCacheMatrix(x)
## m$get()
## #Results
##            [,1]       [,2]
## [1,]  1.0000000 -0.3333333
## [2,] -0.3333333  1.0000000
##
## cacheSolve(m)
##       [,1]  [,2]
##  [1,] 1.125 0.375
##  [2,] 0.375 1.125
##
## cacheSolve(m)
##  getting cached data
##       [,1]  [,2]
##  [1,] 1.125 0.375
##  [2,] 0.375 1.125
