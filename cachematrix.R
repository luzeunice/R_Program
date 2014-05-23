
#The first function, makeMatrix creates a special "matrix", 
#which is really a list containing a function to 
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the mean
#4.get the value of the mean


        makeCacheMatrix <- function(x = matrix()) {
                mat <- NULL
                
                set <- function(y) {
                        x <<- y
                        mat <<- NULL
                }
                
                get <- function() x
                
                setInv <- function(Inve) mat <<- Inve
                
                getInv <- function() mat
                
                list(set = set, get = get,
                
                     setInv = setInv,
                     getInv = getInv)
        }
        

 

## The following function calculates the mean of the special "matrix" 
#created with the above function. However, it first checks to see if
#the mean has already been calculated. If so, it gets the mean from the cache and 
#skips the computation. Otherwise, it 
##calculates the mean of the data and sets the value of the mean in
#the cache via the setmean function.
        
        cacheSolve <- function(x, ...) {
                
                mat <- x$getInv()
                
                if(!is.null(mat)) {
                        message("getting cached data")
                        return(mat)
                }
                
                data  <-  x$get()
                
                mat   <-  solve(data)
                
                x$setInv(mat)
                
                mat 
        }
        
        
        
         
