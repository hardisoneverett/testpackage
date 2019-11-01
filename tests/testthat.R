library(testthat)
library(testpackage)

test_check("testpackage")

expect_error(fars_read("20"),paste0("file '", "20", "' does not exist"))
