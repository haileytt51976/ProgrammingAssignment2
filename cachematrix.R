##for this assignment, I will be creating a couple of functions "makeCacheMatrix" and "cacheSolve"

##According to the assignment, "makeCache" is a "function that creates a special "matrix" object that can cache its inverse."

makeCacheMatrix <- function(x=matrix()){
  p <-NULL
  set<-function(y){
    x<<-y
    p<-NULL
  }
  get <-function()x
  setInverse <- function(inverse)p<<-inverse
  getInverse <- function()j
  list(set =set, get=get,setInverse=setInverse, getInverse=getInverse)
}

##According to the assignment, "cacheSolve" is a "function that computes the inverse of the special "matrix" returned by makeCacheMatrix"
cacheSolve <- function(x, ...){
  p<-x$getInverse()
  if(!is.null(p)){
    message("get cached result")
    return(p)
    mat<-x$get()
    p<-solve(mat,...)
    x$setInverse(p)
    p
  }
}
