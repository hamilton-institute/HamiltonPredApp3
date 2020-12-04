## code to prepare `work_data` dataset goes here
# download the dataset from the ECDC website to a local temporary file
library(dplyr)
httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
          httr::authenticate(":", ":", type = "ntlm"),
          httr::write_disk(tf <- tempfile(fileext = ".csv")))
coronavirus <- read.csv(tf, encoding = "UTF-8-BOM")

work_data <- coronavirus

# str(work_data[, c(1, 5:7, 10, 11)])

# pre-processing and info
work_data$time <- as.Date(work_data$dateRep, "%d/%m/%y")
work_data$Time <- as.numeric(work_data$time) - min(as.numeric(work_data$time)) + 1

ND <- work_data %>%
  dplyr::group_by(countriesAndTerritories) %>%
  dplyr::summarise(N = dplyr::n())

for_time <- shiny_data %>%
  filter(Indicator ==  "Forecast") %>%
  select(time)

for_time <- as.Date(unique(for_time[, 1]), "%d/%m/%y")


work_data <- work_data %>%
  filter(Time < 366) %>%
  arrange(Time, countriesAndTerritories) %>%
  droplevels()

work_data <- work_data %>%
  filter(time %in% c(unique(for_time)))

if (nrow(work_data) == 0) work_data[1, ] <- NA

work_data$Country <- work_data$countriesAndTerritories
work_data$Country <- as.factor(gsub(pattern = "_", replacement = " ",
                                    work_data$Country))
work_data$Indicator <- "Observed"
work_data$Lower10 <- work_data$Upper90 <- work_data$Lower5 <-
  work_data$Upper95 <- work_data$Lower2_5 <- work_data$Upper97_5 <- NA
usethis::use_data(work_data, overwrite = TRUE)
