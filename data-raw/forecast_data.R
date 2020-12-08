load("data-raw/RData/data_forecast3.RData")
data_forecast1 <- data_forecast
data_forecast1$Type  <- "First Validation"
data_forecast1$day2 <- as.factor(data_forecast1$day)
levels(data_forecast1$day2) <- paste(1:7, "ahead")
levels(data_forecast1$country)[c(176, 196, 198)] <-
  c("South Korea", "UK", "USA")
#-----------------------------------------------------------------------
load("data-raw/RData/data_forecast2.RData")
data_forecast2 <- data_forecast
data_forecast2$Type  <- "Second Validation"
data_forecast2$day2 <- as.factor(data_forecast2$day)
levels(data_forecast2$day2) <- paste(1:7, "ahead")
levels(data_forecast2$country)[c(177, 198, 200)] <-
  c("South Korea", "UK", "USA")
#-----------------------------------------------------------------------
load("data-raw/RData/data_forecast1.RData")
data_forecast3 <- data_forecast
data_forecast3$Type  <- "Third Validation"
data_forecast3$day2 <- as.factor(data_forecast3$day)
levels(data_forecast3$day2) <- paste(1:7, "ahead")
levels(data_forecast3$country)[c(178, 199, 201)] <-
  c("South Korea", "UK", "USA")
#-----------------------------------------------------------------------
data_forecast <- rbind(data_forecast1, data_forecast2, data_forecast3)
usethis::use_data(data_forecast, overwrite = TRUE)
