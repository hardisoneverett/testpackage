library(testthat)
library(testpackage)



testthat::expect_identical(make_filename(2013),"accident_2013.csv.bz2")



