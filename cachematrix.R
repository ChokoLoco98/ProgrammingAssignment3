## the overall project is divided in two functions: makecacheMatrix and cacheSolve. 


## "makeCacheMatrix" creates the matrix, get its inverse and cache it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          # get the values
  set <- function (y){
    x <<- y              # "<<- within set it gives to x in the value y
    inv <<- NULL       
  }
  get <-function()x   
  setInverse <- function(inverse) inv<<-inverse    #set the value of the inverse
  getInverse <- function() inv                     #get the value of the inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

##"Cachecolve" takes the inverse matrix created by the previous function and cache it. 
# if the reverse matrix hasn't already been created, "CacheSolve " would do it. 

cacheSolve <- function(x, ...) { 
 inv <- x$getInverse()
 if(!is.null(inv)){                # the function !is.na gives true if the inverse is not a NULL
   message("getting cached data")
   return(inv)                     # gives the inverse
 }
 data <- x$get()
 inv <- solve(data, ...)           # the solve function give the value of the inverse
 x$setInverse(inv)
 inv                               # return the value of the inverse matrix of x
}





