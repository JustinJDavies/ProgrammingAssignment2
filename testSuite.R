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
expect_false(isTRUE(all.equal(testMatrix,testMatrix2)))
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