## makeCacheMatrix() will creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL # initializing inverse matrix as NULL
    set <- function(y){ # 1.set the value of the matrix
        x <<- y   # setting matrix input to x in global environment
        inv_matrix <<- NULL   # resetting cached inverse matrix as NULL and saving in global environment
    }
    get <- function() x # 2.get the value of the matrix. matrix value will be different before/after calling set() function.
    setInverse <- function(inverse) inv_matrix <<- inverse # 3.set the value of the inverse. this will be called by cacheSolve() function.
    getInverse <- function() inv_matrix # 4.get the value of the inverse. getting from initialized (NULL) or cached inverse matrix.
    list(set = set, get = get,  # saving result of calling makeCacheMatrix(). each function is saved as list element and will be called by using '$'
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() computes the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    inv_matrix <- x$getInverse()  # getting saved inverse matrix and saving in inv_matrix
    if(!is.null(inv_matrix)) { # if inv_matrix is not NULL (having cached inverse matrix), then retrieving cached inverse matrix
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()  # if inv_matrix is NULL,getting x (input matrix) and saving in data 
    if(ncol(data) == nrow(data) && det(data) != 0)  {  # if data is a square matrix AND determinant is not zero (condition to have an inverse matrix)
        inv_matrix <- solve(data, ...) # computing inverse matrix using solve() function then saving in inv_matrix
        x$setInverse(inv_matrix) # passing computed inverse matrix to setInverse() function
    } else {
        message("not an invertible matrix") # warning if matrix is not invertible
    }
    inv_matrix  # Return a matrix that is the inverse of 'x'
}