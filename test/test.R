library(fars)
library(testthat)

# Test make_filename with an integer input
expect_equal(make_filename(2013),"accident_2013.csv.bz2")

# Test make_filename with an string input
expect_equal(make_filename("2013"),"accident_2013.csv.bz2")

# Test make_filename with an integer vector input
expect_equal(make_filename(2013:2014),c("accident_2013.csv.bz2","accident_2014.csv.bz2"))


