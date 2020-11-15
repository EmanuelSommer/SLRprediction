library(testthat)
library(SLR.prediction.vis)

test_that("SLR_prediction test", {
  # numeric
  test_vector <- 1:10
  expect_equal(SLR_prediction(test_vector, test_vector), mean(test_vector))
  # data.frame
  expect_equal(SLR_prediction(data.frame(test_vector, test_vector)), mean(test_vector))
  # matrix
  expect_equal(SLR_prediction(cbind(test_vector, test_vector)), mean(test_vector))
})
