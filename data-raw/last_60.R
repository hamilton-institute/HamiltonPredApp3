## code to prepare `DATASET` dataset goes here

last_60 <- fitted_values %>%
  dplyr::filter(as.numeric(day) -
                  max(as.numeric(fitted_values$day)) + 61 > 0) %>%
  dplyr::select(day, country, ar) %>%
  tidyr::pivot_wider(id_cols = 1:2,
              names_from = "country",
              values_from = "ar") %>%
  dplyr::select(-day)

usethis::use_data(last_60, overwrite = TRUE)

