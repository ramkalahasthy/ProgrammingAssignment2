## These functions are written for Coursera Data Science: R Programming course
## Week#3 Assignment
## Venkataraman TK

## this function creates a matrix object that caches its inverse value

makeCacheMatrix <- function(x = matrix()) {
  inverseMat <- NULL
  
  #set the value of the Matrix
  setMat <- function(y) {
    x <<- y
    inverseMat <<- NULL
  }
  
  getMat <- function() x                              #get the value of the Matrix
  setInv <- function(inverse) inverseMat <<- inverse  #set the value of the inverse matrix
  getInv <- function() inverseMat                     #get the value of the inverse matrix
  list(setMat = setMat, getMat = getMat,
       setInv = setInv, getInv = getInv)
}


## this function takes the input of the previous matrix type 
## and determines if any inverse of the input exists or not.
## If inverse matrix is empty, it obtains the original matrix data
## and sets the inverse matrix. 
## Else if inverse matrix contains cached value, it returns the message
## "Fetching Cached Inverse Matrix" and the cached value.

cacheSolve <- function(x, ...) {
  inverseMat <- x$getInv()
  if(!is.null(inverseMat)) {                       #if inverse matrix is not NULL
    message("Fetching Cached Inverse Matrix")      #Type message: Fetching Cached Inverse Matrix 
    return(inverseMat)                             #return the inverse matrix
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMat()                         #get the original Matrix Data 
  inverseMat <- solve(MatrixData, ...)             #use solve function to inverse the matrix
  x$setInv(inverseMat)                             #set the inverse matrix 
  return(inverseMat)  
}
