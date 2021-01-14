library(testthat)
library(psyquest)

test_check("psyquest", filter = "_", reporter = "summary")
