library(fars)
library(testthat)

expected_pattern = "/fars/inst/extdata/accident_2013.csv.bz2"

cat("Finish setting up! Begin test ... ")

# Test make_filename with different type inputs
test_that("make_filename with different type inputs",{
  expect_equal(grep(expected_pattern, make_filename(2013)),1)
  expect_equal(grep(expected_pattern, make_filename("2013")),1)
})

# Test results of fars_summarize_years
dat = fars_summarize_years(2013:2015)

test_that("fars_summarize_years years 2013,2014 and 2015",{
  expect_equal(dat$MONTH[1]  ,   1)
  expect_equal(dat$`2013`[1] ,2230)
  expect_equal(dat$`2014`[1] ,2168)
  expect_equal(dat$`2015`[1] ,2368)
})
