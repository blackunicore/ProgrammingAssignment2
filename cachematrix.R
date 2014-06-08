#cachematrix.R
#the program includes two functions makeCacheMatrix and solveCacheMatix
#first function makeCacheMatrix creates an empty matrix the first time it is called
#and it should be called like this "x<-makeCacheMatrix()"
#then to create a matrix containing numbers call "x$set(numeric vector)"
#the vector has to have a kength that gives an integer then the squareroot of the length is taken
#secoundly there is the function solveCacheMatrix it should be called as follows:
#"solveCacheMatrix(x)" 
#this function will first check if the matrix has already been inverted 
#if is has then it will show the cached inveted matrix 
#if it hasn't been inverted then the function will invert it, save it to the cache and print it


#walkthrough of the function makeCacheMatrix
#first the function makes a matrix called X which is empty and since this is a new matrix
#the inverted matrix variable "invertx" is set to NULL.
#then it creates the function set which when called will create a square matrix,
#this requires the correct input otherwise the function will just adapt the square matrix that it can fill.
#then the function creates all the functions needed to cache and get somthing from the cache.
#lastly it creates a list of all the function that can be called after the main
#function has been run.

makeCacheMatrix <- function(x = matrix()) {
    invertx <- NULL              
    
    set<-function(y) {           
        nrow <- sqrt(length(y))
        ncol <- sqrt(length(y))        
        x <<- matrix(data=y, nrow=nrow, ncol=ncol)
        invertx <<- NULL
    }
    
    get <- function() x          
    cache <- function(inverted) invertx <<-inverted
    getcache <- function() invertx
    list(set = set, get = get, 
         invert = invert, 
         getinvert = getinvert) 
}

#walkthrough of the function solveCacheMatrix
#first the function calls the getcache funtion to accuire the inverted matrix.
#it then checks if the matrix has been inverted, if it has been inveted,
#then it writes "getting cached data" and returns the inverted matrix.
#If the matrix is new and has not been inverted the function gets the matrix with
#the get funtion.
#then it invertes the function useing solve and stores it as the variable "invertx".
#then lastly it caches the inverted matrix and prints the inverted matrix to the console.

solveCacheMatrix <- function(x) {
    invertx <- x$getcache()
    
    if (!is.null(invertx)) {
        message("getting cached data")
        return(invertx)
    }
    
    matrix <- x$get()
    invertx <- solve(matrix)
    x$cache(invertx) 
    invertx
}                
