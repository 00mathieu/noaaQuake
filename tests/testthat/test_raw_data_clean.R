library(testthat)

test_that("Output is data.table",
          expect_that(db <- clean_raw_data(),is_a("data.table")))

