
## This code is public domain.
##
## This example demonstrates caching the inverse matrix once it is
## calculated for the future use.


## This function constructs a matrix object that caches
## the inverse matrix when it is calculated.
##
## @param matrix.value a matrix to create the caching object from
##
## @returns an object containing the matrix and the way to
## calculate (and cache) the inverse of the matrix
makeCacheMatrix <- function(matrix.value = matrix())
{
    # The reference to value will be stored in the resulting object
    matrix.inverted <- NULL

    # We are going to use this to test whether the matrix inverse
    # was calculated more than once
    matrix._timesCalculated <- 0L

    # Constructing the result
    list (
        ## Method which returns the matrix value
        value = function()
            matrix.value,

        ## Lazily calculates and returns the inverted matrix
        ## @params optional arguments for the `solve` method
        inverted = function(...)
        {
            # If we haven't calculated it already, we
            # will now, and we will remember the result
            if (is.null(matrix.inverted)) {
                matrix.inverted <<- solve(matrix.value, ...)

                # for tests
                matrix._timesCalculated <<- 1 + matrix._timesCalculated
            }

            matrix.inverted
        },

        ## Method to set the value.
        ## @param new_value new value for the matrix
        ## @note  This is requested by the problem specification,
        ##        but it is not used.
        setValue = function(new_value)
        {
            # main object fields
            matrix.value    <<- new_value
            matrix.inverted <<- NULL

            # for tests
            matrix._timesCalculated <<- 0L
        },

        ## Method to set the inverted value.
        ## @param new_inverted new value for the inverted matrix
        ## @note  It is requested by the problem specification,
        ##        but is is not needed - the inverted matrix cache
        ##        will be calculated as needed. Having this method
        ##        breaks encapsulation as it can be used to set an
        ##        incorrect inverse matrix.
        setInverted = function(new_inverted)
        {
            matrix.inverted <<- new_inverted
        },

        ## @returns the number of times the inverse has
        ## been calculated
        tests.timesCalculated = function()
            matrix._timesCalculated
    )
}


## This function gets a matrix object constructed by
## makeCacheMatrix and returns the matrix inverse.
## @param matrix_cache matrix object to return inverse of
## @param arguments to pass to `solve`
cacheSolve <- function(matrix_cache, ...)
{
    matrix_cache$inverted(...)
}


####################################################
# Functions for testing:                           #
####################################################

## Create a random square matrix (+cache).
## @param dimension dimension of the square matrix to create
## @returns the matrix object containing the newly created matrix
tests.makeRandMatrix <- function(dimension)
    makeCacheMatrix(matrix(rexp(dimension ^ 2, rate=.1), ncol=dimension))


## Assert function - stops with an error message if the
## test fails
tests.assert <- function(test, message)
{
    if (!test) {
        stop(message)
    }
}


## Run tests for the specified matrix
## @param obj matrix object to run tests for
## @param dimension dimension of the matrix
tests.testMatrix <- function(obj, dimension)
{
    tests.assert(obj$tests.timesCalculated() == 0,
                "Cache was calculated before the inverse matrix was requested")

    # Calculating the inverse and testing whether the inverse
    # matrix is correctly calculated
    mat <- obj$value()
    inv <- cacheSolve(obj)
    mul <- mat %*% inv

    tests.assert(obj$tests.timesCalculated() == 1,
                "Cache should have been only calculated once")

    tests.assert(all(diag(dimension) == round(cor(mul), 0)),
                "Inverse matrix not calculated correctly")

    # Trying again
    inv <- cacheSolve(obj)

    tests.assert(obj$tests.timesCalculated() == 1,
                "Cache was calculated more than once")

    # And again
    inv <- cacheSolve(obj)

    tests.assert(obj$tests.timesCalculated() == 1,
                "Cache was calculated more than once")

}


## This function creates a bunch of differently sized
## matrices and tests whether the cache works
tests.testCache <- function()
{
    for (i in 5:50) {
        # This should not generate matrices that do not have
        # inverse. That is, probability of it is zero. If that
        # happens, the test will fail. But that is ok for an
        # example Coursera programming assignment like this one.
        obj <- tests.makeRandMatrix(i)

        tests.testMatrix(obj, i)

        # Setting new values to `obj` and testing whether
        # the value setter works correctly
        for (retry in 1:5) {
            temp <- tests.makeRandMatrix(i)
            obj$setValue(temp$value())

            tests.testMatrix(obj, i)
        }
    }

    print("All tests passed")
}
