
<!-- README.md is generated from README.Rmd. Please edit that file -->

# deepredeff <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/deepredeff)](https://cran.r-project.org/package=deepredeff)
[![CRAN\_Status\_Badge](https://github.com/ruthkr/deepredeff/workflows/pkgdown/badge.svg)](https://ruthkr.github.io/deepredeff/)
<!-- badges: end -->

**deepredeff** is a package to predict effector protein given amino acid
sequences. This tool can be used to predict effectors from three
different pathogens, which are oomycete, fungi, and bacteria.

## Installation

The development version can be installed from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("ruthkr/deepredeff")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Specify the conda environment 
reticulate::use_condaenv("tensorflow", conda = "/usr/local/Caskroom/miniconda/base/bin/conda")

# Load the package
library(deepredeff)

# Define the fasta path from the sample data
bacteria_fasta_path <- system.file("extdata", "example", paste0("bacteria_sample", ".fasta"), package = "deepredeff")

# Predict the effector candidate 
pred_bacteria_fasta_input <- deepredeff::predict_effector(
  input = bacteria_fasta_path,
  model = "bacteria"
  )
#> Loaded models: cnn_gru, cnn_lstm, gru_emb, lstm_emb for pathogen bacteria.
#> Loaded models successfully!

# View results
pred_bacteria_fasta_input %>% 
  dplyr::mutate(sequence = stringr::str_sub(sequence, 1, 50)) %>% 
  knitr::kable()
```

| name          | sequence                                           |      prob |
| :------------ | :------------------------------------------------- | --------: |
| A0A0Q0BGR4\_1 | MGNICGTSGSHYVYSPPVSPRHVSGSSTPVHSVGGQGLTSVYQLSAEARD | 0.9916961 |
| ALS93546\_0   | MIRRIAGVLLSVLAWAGPAHATDQLPDLIQIDGQQATLLAEPLSSPLDDP | 0.5174671 |
