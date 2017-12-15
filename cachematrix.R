
#some functions involve high computational time.  
#In such cases it is usefull to use data fom previous computations for data with same values.
#this not only reduces computational time but also conserves the system's resources.
#thus caching data and using it later is very usefull.
#the below functions perform the inverse of a matix and cache its value,
#so that this cached data can be used when inverse for the same matrix has to be calculated again
#hence reducing computational time and the resources involved.



# This makeCachematrix  is used for creating the inverse of the input matrix
#It has four sub functions namely 
#set()- which takes in the input matrix 
#get()-for getting the matrix into the function
#setinv()-for calculating the inverse of the matrix using the solve() function
#getinv()-for stoing the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y) {
    x <<- y
    p <<- NULL
  }
  get <- function() x
  setinv <- function() p <<- solve(x)
  #solve function is used fo calculating the inverse of the matrix
  #inverse of the matrix can be calculated only when det!=0
  getinv <- function() p
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#this function checks whether the inv of the given matrix is 
#already present in the cached data or not
#if the inv for the required matrix is not present, then it calculates the inv and 
#stores it in the cached data

cacheSolve <- function(x, ...) {
  p <- x$getinv()
  if(!is.null(p)) {
    message("getting cached data")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinv(p)
  p
  ## Return a matrix that is the inverse of 'x'
}

#code for the working of the above functions

working<-makeCacheMatrix()
working$set(d)
working$get()


working$setinv()
working$getinv()

cacheSolve(working)

#data used fo the working 

a<-matrix(1:9,3,3)#for a singular matrix (det=0) the inverse cannot be calculated 
b<-matrix(c(1,2,3,4,2,3,6,7,3,56,7,21,3,56,77,88),4,4)
c<-matrix(c(1,2,3,4,2,3,6,7,3,56,7,21,3,56,77,88),4,4)
d<-matrix(c(57,23,3,55,5,21,3,4,5,13,23,65,7,35,8,35,22,3,32,11,22,33,44,23,9),5,5)
