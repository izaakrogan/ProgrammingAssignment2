# The below functions are able to cache the results of Inverting (i.e. calling solve) a matrix
# solve will be performaed on a new matrix once and the result of calling solve will be cahed in
# an object m. The inverse of a matrix can then be retrieved multiple times without needing to
# compute the answer again.

# makeCacheMatrix is a function that stores a list of functions.
# four 'methods' are made available to the calling environment: set(), get(), setsolve(), getsolve()
# x is a matrix obj given from the calling environment.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL

    set <- function(y) {
      x <<- y
      m <<- NULL
    }

    get <- function() x

    setInv <- function(Inv) m <<- Inv

    getInv <- function() m

    list(set = set, get = get, setInv = setInv, getInv = getInv)

}

# cacheSolve checks if x has already been 'solve[d]()' and cached.
# If true, the cached value will be returned.
# If not the inverse is calculated with solve and the results are then cached i.e. assigned
# to m in setInv
# x is a passed in makeCacheMatrix obj

cacheSolve <- function(x, ...) {

    print(x)

    m <- x$getInv()

    # If data is cached return cached value
    if(!is.null(m)) {
        message("getting cached data...")
        return(m)
    }

    data <- x$get()       # get data from makeCacheMatrix obj

    m <- solve(data, ...) # solve for data

    x$setInv(m)           # take the result of computing inverse and cache inverse using setInv

    m                     # returned cached value

}

# Added for fun not a requirment
a <- matrix(rnorm(100), nrow = 10)
# return object of functions with a as as the argument
cash <- makeCacheMatrix(a)
# check cash get returns matrix
identical(cash$get(), a)  # TRUE
identical(cash$getInv(), NULL)  #TRUE
# run cash solve on cashed matrix
# calls setInv on makeCacheMatrix
cacheSolve(cash)
identical(cash$getInv(), NULL) #FALSE
# second time m is not NULL so cachSolve returns the value
# of getInv i.e. gets cached matrix
cacheSolve(cash)

# Just for fun let's do a second matrix
b <- matrix(rnorm(100), nrow = 10)
# check a and b are different matrices
identical( a, b ) # FALSE
cash2 <- makeCacheMatrix(b)
identical(cash2$get(), b) # TRUE
identical(cash2$getInv(), NULL) #TRUE
cacheSolve(cash2)
identical(cash2$getInv(), NULL) #FALSE
# second time m is not NULL so cachSolve returns the value
# of getInv i.e. gets cached matrix getting cached data... is logged to console
cacheSolve(cash2)
identical(cacheSolve(cash), cacheSolve(cash2)) # FALSE
