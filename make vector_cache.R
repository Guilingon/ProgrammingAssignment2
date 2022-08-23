makeVector <- function(x = numeric()) {
  m <- NULL #begins by setting the mean to NULL as a placeholder for a future value
  set <- function(y) {   
    x <<- y
    m <<- NULL
  }  #defines a function to set the vector, x, to a new vector, y, and resets the mean, m, to NULL
  get <- function() x #returns the vector, x
  setmean <- function(mean) m <<- mean #sets the mean, m, to mean
  getmean <- function() m #returns the mean, m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}  #returns the 'special vector' containing all of the functions just defined

## Functions to cache inverse of a matrix

cachemean <- function(x, ...) {  ## Creation of matrix object
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
x<-makeCacheMatrix(matrix(1:4,nrow = 2,ncol = 2))
makeCacheMatrix <- function(x = matrix()) {
  inversa<-NULL                   #initializing inverse as NULL
  set<-function(y){
    x<<-y
    inversa<<-NULL
  }
  get<-function(){x}               #function to get matrix x
  setInversa<-function(inversacalculada){inversa<<-inversacalculada}
  getInversa<-function(){inversa}
  list(set = set, get = get,
       setInversa = setInversa,    #function to get the inverse of the matrix
       getInversa = getInversa)
}
x<-makeCacheMatrix(matrix(1:4,nrow = 2,ncol = 2))
x
x$get()
cacheSolve <- function(x, ...) {        #The following function will get the inverse in cache
  inversa<-x$getInversa()                #Getting the return value of inversa
  if(!is.null(inversa)){                #condition error
    message("getting cached data")
    return(inversa)                      #return cache if is not NULL
  }
  data<-x$get()
  inversa<-solve(data,...)          #If IS null, then get the inverse (with solve()) and cache it
  x$setInversa(inversa)
  inversa
}
cacheSolve(x)