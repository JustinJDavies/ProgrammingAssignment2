source("cachematrix.R")

# install.packages("testthat")
library(testthat)

# Unit Tests
testMatrix <- matrix(data = 1:4, nrow = 2)
testMatrix2 <- matrix(data = 3:6, nrow = 2)

testInverse <- solve(testMatrix)
testInverse2 <- solve(testMatrix2)

testCacheMatrix <- makeCacheMatrix(testMatrix)
# verify state
class(testCacheMatrix1)
str(testCacheMatrix)
str(testCacheMatrix$get())

actual <- cacheSolve(testCacheMatrix)
expected <- testInverse
expect_equal(actual, expected)
# should send cache message second time
expect_equal(testInverse, cacheSolve(testCacheMatrix))

# if we change the matrix to a non-identical one...
expect_false(isTRUE(all.equal(testMatrix, testMatrix2)))
testCacheMatrix$set(y = testMatrix2)

actual <- cacheSolve(testCacheMatrix)
expected <- testInverse2
expect_equal(actual, expected)

# if we again set the matrix, but to the same value ... (we want to see message saying it used cache)
testCacheMatrix$set(y = testMatrix2)

actual <- cacheSolve(testCacheMatrix)
expected <- testInverse2
expect_equal(actual, expected)



# Test message
testCacheMatrix2 <- makeCacheMatrix(testMatrix)
# Should not produce a message on set as inverse is not yet calculated :
testCacheMatrix2$set(testMatrix)
testCacheMatrix2$getinv()


cacheSolve(testCacheMatrix2)
testCacheMatrix2$getinv()

testCacheMatrix2$set(testMatrix)
testCacheMatrix2$getinv()

# Don't allow non-matrix objects, for example (not exhaustive...) :
expect_silent(testCacheMatrix <- makeCacheMatrix(matrix()))
expect_warning(testCacheMatrix <-
                   makeCacheMatrix(data.frame()), regexp = "Must pass an object of class matrix")
expect_warning(testCacheMatrix <-
                   makeCacheMatrix(vector()), regexp = "Must pass an object of class matrix")
expect_warning(testCacheMatrix <-
                   makeCacheMatrix(list()), regexp = "Must pass an object of class matrix")
expect_warning(testCacheMatrix <-
                   makeCacheMatrix(factor()), regexp = "Must pass an object of class matrix")



# Test we can create two cacheMatrix objects and the second doesn't intefere with the first

tm1 <- makeCacheMatrix(testMatrix)
tm2 <- makeCacheMatrix(testMatrix2)


expect_silent(actual <- cacheSolve(tm1))
expected <- testInverse
expect_equal(actual, expected)

expect_silent(actual <- cacheSolve(tm2))
expected <- testInverse2
expect_equal(actual, expected)

# Should pass AND also send message "getting cached value"
# How do I test two things?
expect_message(actual <- cacheSolve(tm1), regexp = "getting cached value")
expected <- testInverse
expect_equal(actual, expected)







