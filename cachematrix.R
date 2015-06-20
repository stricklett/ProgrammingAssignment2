## The functions cacheSolve() and makeCacheMatrix() are used to create a special list object
## that stores a matrix and cache's its inverse.
## 
## Examples:
##    cm <- makeCacheMatrix(x)  # create the special list object 'cm'
##    xInv <- cacheSolve(cm)    # 'xInv' is the inverse of 'x'
##    
##
## The function makeCacheMatrix(x) accepts a single arguement 'x', 
## where 'x' is a square invertable matrix.and
## returns an special list object. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The function cacheSolve(cm, ...) accepts an arguement 'cm',
## where 'cm' is a special list oject created by the function makeCacheMatrix(x), and
## returns a matrix that is the inverse of 'x' 

cacheSolve <- function(cm, ...) {
    m <- cm$getInverse()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- cm$get()
    m <- solve(data)
    cm$setInverse(m)
    m 
}

