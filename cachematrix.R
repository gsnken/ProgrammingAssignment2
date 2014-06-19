## Ass part of the ProgrammingAssignment2, we have to find inverse of an given Matrix.
## We have to cache the Matrix as well as its inverse 
## So that, next time the inverse or the matrix are needed, the are fetched from the cahce. 
## I have written three functions - 
##   cacheSolve - Checks if the inverse was already calculated, if not then calculate it
##   makeCacheMatrix - manages the read and write to cache for the matrix and its inverse 
##   trial - shows a sample test case for using the two - not strictl a part of the assignment


## trial function here is a sample of how the two functions can be called and tested 
## you may run-
## >trial()
## from the Rstudio to test the code
trial <- function() {
    # First instantiate the matrix in cache with makeCacheMatrix
    temp <- makeCacheMatrix(matrix(c(1,-0.25,-0.25,1), nrow=2, ncol=2))
    print("Given Matrix was -")
    print(temp$get())
    
    #then invert it
    ans <- cacheSolve(temp)
    #first time calculates (clean env before running twice)
    print("Inverse is - ")
    print(ans)
    
    #invert it again 
    #second time on gets from cache 
    ans <- cacheSolve(temp)
    #it says it is getting cached data
    print(ans)

    #check if the inveres is correct  
    m1 <- temp$get() %*% ans
    print("multiplication of the above two is - ")
    m1
}

## makeCacheMatrix associates four functions with a given matrix
## these do not manipulate the Data but only read and write from cache
## get() - fetches the data of the Matrix (NULL if not already assigned)
## set(Data) - stores the input Data into cache and makes inverse of this matrix NULL
## getinv() - fetches the inverse of the Data matrix - it may be NULL
## setinv(invData) - stores the given inverted Data in cache - note that it does not calculate or validate.
## The cache variables are called M (the Data Matrix) and invM (the inverse Matrix) 
makeCacheMatrix <- function(M = matrix()) {
 #intialise inverse
    invM <- NULL
 #commit Data to cache if set or constructor is called
    set <- function(y) {
        M <<- y
        invM <<- NULL
    }
 #return Data from cache if get is called
    get <- function() {
        M
    }
 #commit inverse to cache if setinv is called
    setinv <- function(inv) {
        invM <<- inv
    }
 #return the inverse from Cache if getinv is called
    getinv <- function() {
        invM
    }
#Associate the above four fuctions with list of names to called from outside    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve function checks if the inverse for the given Matrix is already
## calculated and stored in cache. - If so, it merely fetches it and returns it.
## if not alread calcualted, the inveres is calculated, commited to cache 
## and returned to calling function.
cacheSolve <- function(x, ...) {
    #fetch the inverse and check if it is not null 
    invX <- x$getinv()
    if(!is.null(invX)) {
        #no need to calculate again
        message("getting cached data")
        return(invX)
        #return means code below this point does not get executed for this if condition
    }
    #the inverse was NULL , so we get the Matrix
    data <- x$get()
    #calculate the inverse
    invX <- solve(data)
    #store the inverse in cache
    x$setinv(invX)
    #return it to calling routines
    return(invX)
}


