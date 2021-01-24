##KavitaDrake writing code for assignment 2, caching the inverse of a matrix.

#This was a useful page for learning about inverse matrices:
#https://www.mathsisfun.com/algebra/matrix-inverse.html
#Here are sample data inputs for testing:  
#testdata <- matrix(c(4, 2, 7, 6), 2, 2)
#testdata2 <- matrix(c(3, 3.2, 3.5, 3.6), 2, 2)
#testdata3 <- matrix(c(3, 2, 0, 0, 0, 1, 2, -2, 1), 3, 3)

#makeCacheMatrix generates the inverse matrix for given data, and stores it
#in our cache
#For our test data, we would use these commands:
#td <- makeCacheMatrix(testdata)
#td2 <- makeCacheMatrix(testdata2)
#td3 <- makeCacheMatrix(testdata3)
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL #name it 'im' for 'inverse matrix,' initializing it w/ NULL value
    set <- function(y) { #We're making the 1st func to re/set values of y and im
        x <<- y #setting the variable x, which is generally the user input, to 
                #value passed here, which is y.
        im <<- NULL #If we're starting with a new x, we need to clear im.
    }
    get <- function() x #This simply returns the value of x when queried
    setim <- function(solve) im <<- solve #Set the im value through solve
    getim <- function() im #Return the value of im
    list(set = set, get = get, setim = setim, getim = getim) 
     #make a list with the functions named like themselves
}
#When a user wants to recall or generate anew the inverse matrix, she calls
#the cacheSolve function. In our test cases, this would be the next step:
#cacheSolve(td) and cacheSolve(td2) and cacheSolve(td3)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getim() #this will be NULL if the user hasn't called it before, or
                    #has a different version of x, needing a new inverse
    if(!is.null(im)) {
        message("I'll use the old inverse matrix, ie cached")
        return(im) #Return the one in storage
    }
    data <- x$get() #If we're generating a new one, use this info.
    im <- solve(data, ...) #If it's null, generate the inverse matrix here.
    x$setim(im) #Store the im value. I'm not actually sure if the inverse is
    #getting solved here/previous line or in the setim line of the last
    #function. Will be researching. Makes sense that it would be the previous
    #one, but otherwise why use 'solve' here?
    im #Let the user know what the inverse matrix is
}
#This should be the output for our test cases:
#testdata inverse matrix:
#      [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4
#testdata2 inverse matrix:
#       [,1] [,2]
# [1,] -9.00  8.0
# [2,]  8.75 -7.5
#testdata3 inverse matrix:
#      [,1] [,2] [,3]
# [1,]  0.2  0.2    0
# [2,] -0.2  0.3    1
# [3,]  0.2 -0.3    0