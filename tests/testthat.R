library(testthat)
library(deepredeff)
library(tensorflow)

test_check("deepredeff", reporter = c('progress', 'fail'))
