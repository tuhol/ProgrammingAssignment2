## The idea of those two functions is to return the inverse matrix if that exists and
## not to compute the matrix if nothing changed


## Function will return the list of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverseofmatrix <- NULL #seting value of inverse matrix to Null
  set <- function(y){ #Function set for caching the value
    X<<-y
    inverseofmatrix <<-NULL # if the matrix was change inverseofmatrix set to Null 
  }
  get <- function() x #returns value of inverse
  setinverse <- function(solve) inverseofmatrix <<- solve #calculates the inverse of the matrix
  getinverse <- function() inverseofmatrix # gets the inverse
  list(set = set, get = get,     # product of the whle function
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function will return the cache of matrix if needed

cacheSolve <- function(x, ...) {
  inverseofmatrix <- x$getinverse()
  if(!is.null(inverseofmatrix)) {                 
    message("getting cached data - Inverse of the matrix") #if the inverse matrix exists then we return it
    return(inverseofmatrix)
  }
  d<- det(x$get()) # check if we can inverse the matrix (det!=0)
  if(d != 0){  
    data <- x$get()                               
    inverseofmatrix <- solve(data, ...) #calculating the inverse
    x$setinverse(inverseofmatrix)
    inverseofmatrix
  } else {print("No inverse possible")}
}
