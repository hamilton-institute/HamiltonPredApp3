library(dplyr)
forecast_summary <- data_forecast %>%
  group_by(day) %>%
  summarise(correlation = cor(Y_obs, Y_forecast),
            concordance = DescTools::CCC(Y_obs, Y_forecast)$rho.c[1]$est,
            accuracy = DescTools::CCC(Y_obs, Y_forecast)$rho.c[1]$est/
              cor(Y_obs, Y_forecast),
            log10_rss = log10(sum((Y_obs - Y_forecast)^2)))

data_ccc <- forecast_summary %>%
  tidyr::gather("Type",  "Value",  correlation, concordance, accuracy)
data_ccc$Method <- as.factor(data_ccc$Type)
levels(data_ccc$Type) <- c("CCC", "r", "Cb")
data_ccc$day2 <- rep(seq(1, 7, 1), 3 * 3)
data_ccc$Type <- rep(gl(3, 7, labels = c("First Validation",
                                         "Second Validation", "Third Validation")),3)
use_data(data_ccc, overwrite = TRUE)
