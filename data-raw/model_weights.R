## code to prepare `model_weights` dataset goes here
library(dplyr)

bacteria_weight_paths <- Sys.glob(here::here("data-raw", "bacteria_*.csv"))

bacteria_weight_names <- gsub(".*\\/bacteria_|.csv", "", bacteria_weight_paths)

bacteria_weights <- bacteria_weight_paths %>%
  purrr::map(
    .f = function(x) {
      weight <- x %>%
        data.table::fread() %>%
        tail(n = 3L) %>%
        dplyr::filter(acc == max(acc)) %>%
        dplyr::slice(1) %>%
        dplyr::pull(acc)

      return(weight)
    }
  ) %>%
  `names<-`(bacteria_weight_names)

usethis::use_data(bacteria_weights, overwrite = TRUE, internal = TRUE)
