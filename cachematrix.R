## These functions are used in tandem to calculate the inverse of a given matrix, 
## cache the inverse of that matrix, and then call the cached inverse. 


## This function creates a list of functions that can be applied to a matrix to set its values (set), 
##get the values it has been assigned (get), assign the inverse of the matrix to an empty "inversematrix" 
##variable within the parent function (setinverse), and call the "inversematrix" variable within the parent function (getinverse). 
##It is intended to be used with the cacheSolve function.  

#This function is intended to be used with matrices.

makeCacheMatrix <- function(x = matrix()) {
  
  #create empty matrix that inverse can be assigned to    
  inversematrix <- matrix()
  
  #set assigns input matrix to the x variable in the parent function and resets the inversematrix variable 
  #to NA so that each time x is set, cached values of inversematrix are removed
  set <- function (input) {
    x <<- input
    inversematrix <<- matrix()
  }
  
  #get returns the matrix that has been assigned to x. It is useful to be able to call this 
  #matrix in other functions that call makeCacheMatrix as an argument.
  get <- function() x
  
  #given a matrix x that is invertible, setinverse will use solve to calculate its inverse and then assign
  #it to the variable inversematrix in the parent function
  setinverse <- function() inversematrix <<- solve(x)
  
  #getinverse will return the value of the variable inversematrix, as defined in the parent function
  getinverse <- function() inversematrix
  
  #all of these functions are assigned to a list so they can easily be called on the matrix x, ex. x$set
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}

## This function takes a list created by calling the function makeCacheMatrix on a matrix and either
## calculates and returns the inverse of that matrix or calls the cached inverse of the matrix, if it exists. 


cacheSolve <- function(x, ...) {
     
        #assign matrix inversematrix in parent function to actualinversematrix variable
        actualinversematrix <- x$getinverse()
        
        #if there are values in inversematrix (if it's cached), let the user know the data is being retrieved 
        #and return the values
        if(!is.na(actualinversematrix[1,1])) {
        message("getting cached data")
        return(actualinversematrix)
        }
        
        #if inversematrix is na (no values have been cached), calculate the inverse of the matrix and 
        #use setinverse to assign it to the inversematrix value so it's cached and can be called in 
        #future runs of cacheSolve()
        data <- x$get()
        actualinversematrix <- solve(data)
        x$setinverse()
        actualinversematrix
}


