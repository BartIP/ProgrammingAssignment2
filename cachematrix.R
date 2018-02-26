## 2 Fuctions to cache and compute inverse of matrix


## 1 Base for calculations, create special matrix object with ability to cache own inverse

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <-NULL
    set <-function(x) {
      mtx <<-x;
      inverse <<-NULL;
    }
    get <-function() return(mtx)
    setinv <-function(inv) inverse <<-inv
    getinv <-function() return(inverse)
    return(list(set = set,get = get, setinv = setinv, getinv = getinv))
}


## 2 Function to compute inverse of matrix from function 1 (abowe) 
##   in case of having already calculated inverse and under assumption nothing has changed
##   this function should retrive it from cache

cacheSolve <- function(x, ...) {
        inverse <-mtx$getinv()
        if(!is.null(inverse)) {
            message("From cache....")
            return(inverse)
        }
        data <-mtx$get()
        inverse <- solve(data, ...)
        mtx$setinv(inverse)
        return(inverse)
        
}

