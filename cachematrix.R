makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m<<-NULL   
  }
  
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() {
    inverse <- solve(x)
    inverse
  }
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv) 
}


  



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m1 <- x$getInv()
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  data <- x$get()
  m1 <- solve(data, ...)
  x$setInv(m1)
  m1
  }
