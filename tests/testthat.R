library(testthat)
# reticulate::use_condaenv('r-reticulate')
library(deepredeff)

test_check("deepredeff", reporter = c('progress', 'fail'))
