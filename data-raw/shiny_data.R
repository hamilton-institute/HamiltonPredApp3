## code to prepare `shiny_data` dataset goes here
load("data/shiny_data.rda")
shiny_data$Country <- as.factor(gsub(pattern = "_", replacement = " ",
                                     shiny_data$Country))
shiny_data$Indicator <- as.factor(shiny_data$Indicator)
levels(shiny_data$Indicator) <- c("Observed", "Forecast")

usethis::use_data(shiny_data, overwrite = TRUE)
