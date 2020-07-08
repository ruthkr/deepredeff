library(testthat)
library(deepredeff)

test_check("deepredeff", reporter = c("progress", "list", "fail"))
