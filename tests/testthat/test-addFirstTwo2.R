# context("Correct somewhat") context is deprecated

test_that("first two numbers are added", {
  expect_equal(AddFirstTwo(c(-1,-1)), -2)
})

test_that("only first two numbers are added", {
  expect_equal(AddFirstTwo(c(0,1,5)), 1)
})
