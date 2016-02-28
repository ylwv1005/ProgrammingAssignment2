## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set,get=get,
       setinverse=setinverse,getinverse=getinverse)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x
## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition
## Will try to load corpcor library and if it's not installed will try to install the library

cacheSolve <- function(x, ...) 
{
  if(require("corpcor")){
    print("corpcor is loaded correctly")
  } else {
    print("trying to install corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed and loaded")
    } else {
      stop("could not install corpcor")
    }
  }
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("matrix is in memory")
    return(inverse)
  }
  message("inverse is not in memory so the inverse (if exist) is gonna be computed")
  data <- x$get()
  inverse <- pseudoinverse(data, ...)
  x$setinverse(inverse)
  inverse
}
