## -------------------------->Homework Week 4

## cache the inverse of matix

#this Function will assign and cache the matrix
makeCacheMatrix <- function(x = matrix()) {
   p<- NULL
   set<- function(r) {
      x<<- r
      p<<- NULL
   }
   get <- function() x
   setInverse<- function(inverse) p<<- inverse 
   getInverse<- function() p
   list (set = set, get= get,
         setInverse= setInverse,
         getInverse= getInverse)
}

## Here we Inverse
cacheSolve <- function(x, ...) {
        p<- x$getInverse()
        #if it's not 'null' print out the matriX
        if (!is.null(p)) {
           message("Looks like we got it here...")
           return(p)
        }
        #Else, here we go making it inversed
        data<- x$get()
        p<- solve(data, ...) 
        x$setInverse(p)
        return(p)
}
