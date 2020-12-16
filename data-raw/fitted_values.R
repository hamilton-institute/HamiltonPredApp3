## code to prepare `fitted_values` dataset goes here
load("data/fitted_values.rda")
fitted_values$country <- as.factor(fitted_values$country)
fitted_values$country <- as.factor(gsub(pattern = "_", replacement = " ",
                                        fitted_values$country))

usethis::use_data(fitted_values, overwrite = TRUE)
