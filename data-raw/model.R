
httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
          authenticate(":", ":", type = "ntlm"),
          write_disk(tf <- tempfile(fileext = ".csv")))
coronavirus <- read.csv(tf, encoding = "UTF-8-BOM")

work_data <- coronavirus

#str(work_data[, c(1, 5:7, 10, 11)])

# pre-processing and info
work_data$time <- as.Date(work_data$dateRep, "%d/%m/%y")
work_data$Time <- as.numeric(work_data$time) - min(as.numeric(work_data$time)) + 1

ND <- work_data %>%
  group_by(countriesAndTerritories) %>%
  summarise(N = n())

work_data <- work_data %>%
  filter(Time < 366) %>%
  arrange(Time, countriesAndTerritories) %>%
  droplevels()

## creating the data matrix
country_matrix <- work_data %>%
  pivot_wider(id_cols = countriesAndTerritories,
              names_from = Time,
              values_from = cases)
country_names <- country_matrix %>%
  pull(countriesAndTerritories)
country_matrix <- country_matrix[,-1] %>%
  as.matrix
row.names(country_matrix) <- country_names

country_matrix[which(is.na(country_matrix))] <- 0
country_matrix[which(country_matrix < 0)] <- 0
country_matrix_original <- country_matrix


## JAGS model
model <- "model_forecast.txt"
jagsscript <- cat("
  model {
    for(country in 1:Ncountry) {
    #-------------------------------------------------------------------
    # time t = 1
    #-------------------------------------------------------------------
      Y_pred[country,1] <- Y[country,1]
      ar[country, 1] ~ dnorm(0, tau_ar)
    #-------------------------------------------------------------------
    # time t = 2:T
    #-------------------------------------------------------------------
      for(t in 2:N[country]) {
        Y[country,t] ~ dnegbin(p[country,t], r)
        Y_pred[country,t] ~ dnegbin(p[country,t], r)
        p[country,t] <- r/(r + mu[country,t])
        mu[country,t] <- exp(ar[country,t] + Omega[country, t])
        ar[country, t] ~ dnorm(phi[country, t] * ar[country, t-1], tau_ar)
        phi[country,t] <- b[country,1] +
                          b[country,2] * Time[1,t] +
                          b[country,3] * Time[2,t]
        Omega[country,t] <- lambda[country,t] * omega[country,t]
        lambda[country,t] ~ dbern(pi)
        omega[country,t] ~ dnorm(0, tau_omega)
      }
    #-------------------------------------------------------------------
    # forecast
    #-------------------------------------------------------------------
      for(j in 1:n_ahead){
        Y_forecast[country,j] <- Y_pred[country,
                                        N[country] - n_ahead + j]
      }

    #-------------------------------------------------------------------
    # priors for random effects
    #-------------------------------------------------------------------
      for(i in 1:3) {
        b[country,i] ~ dnorm(beta[i], tau_b[i])
      }
    }
    #-------------------------------------------------------------------
    # priors
    #-------------------------------------------------------------------
    r ~ dgamma(0.001, 0.001)
    psi <- pow(r, -1)
    for(i in 1:3) {
      tau_b[i] ~ dgamma(0.001, 0.001)
      sd_b[i] <- pow(tau_b[i], -1/2)
    }
    beta[1:3] ~ dmnorm(mu_beta[1:3], tau_beta[1:3,1:3])
    tau_ar ~ dgamma(0.001, 0.001)
    sd_ar <- pow(tau_ar, -1/2)
    tau_omega ~ dgamma(0.001, 0.001)
    sd_omega <- pow(tau_omega, -1/2)
    pi ~ dunif(0, 1)
  }
", file = model)

# initialization
initfunction <- function(chain) {
  return(switch(chain,
                "1" = list(
                  .RNG.name="base::Super-Duper",
                  .RNG.seed=1),
                "2" = list(
                  .RNG.name="base::Super-Duper",
                  .RNG.seed=2),
                "3" = list(
                  .RNG.name="base::Super-Duper",
                  .RNG.seed=3),
                "5" = list(
                  .RNG.name="base::Super-Duper",
                  .RNG.seed=3),
                "4" = list(
                  .RNG.name="base::Super-Duper",
                  .RNG.seed=3)
  ))
}

# JAGS data
n_ahead <- 7
country_matrix <- country_matrix_original
country_matrix <- cbind(country_matrix, matrix(NA, ncol = n_ahead, nrow = nrow(country_matrix)))
colnames(country_matrix) <- 1:ncol(country_matrix)
n_days <- apply(country_matrix, 1, length)

## creating orthogonal polynomials
Time_poly <- poly(1:ncol(country_matrix), 2)
Time_matrix <- t(Time_poly)

mu_beta <- rep(0, 3)
tau_beta <- diag(rep(0.001, 3))

jags_data <- list("Y" = country_matrix,
                  "N" = n_days,
                  "mu_beta" = mu_beta,
                  "tau_beta" = tau_beta,
                  "Time" = Time_matrix,
                  "n_ahead" = n_ahead,
                  "Ncountry" = nrow(country_matrix))

# parameters to be monitored
jags_params <- c("beta","sd_b","sd_ar","pi","sd_omega","psi",
                 "Y_forecast", "ar")

# run parallel JAGS
nChains <- 5
nAdaptSteps <- 1000
nBurninSteps <- 6000
nThinSteps <- 25
nUseSteps <- 3000

usethis::ui_todo("Running model...")
runJagsOut <- run.jags(method = "parallel",
                       model = model,
                       monitor = jags_params,
                       data = jags_data,
                       n.chains = nChains,
                       adapt = nAdaptSteps,
                       burnin = nBurninSteps,
                       sample = ceiling(nUseSteps/nChains),
                       thin = nThinSteps,
                       summarise = FALSE,
                       plots = FALSE,
                       inits = initfunction)
# coda samples - MCMC
coda_samples <- as.mcmc.list(runJagsOut)
# parameter estimates
estimates <- MCMCsummary(coda_samples[,1:10], round = 4)
estimates
#=======================================================================
# AR Process
#=======================================================================
load("data/fitted_values.rda")
ar_pred <- MCMCsummary(coda_samples, params = "ar")
fitted_values_new <- data.frame("ar" = ar_pred$mean,
                            "ar_low" = ar_pred$`2.5%`,
                            "ar_upp" = ar_pred$`97.5%`,
                            country = rep(rownames(country_matrix),
                                          ncol(country_matrix)),
                            day = (min(work_data$time) - 1) +
                              rep(1:ncol(country_matrix),
                                  each = nrow(country_matrix)))
fitted_values_new <- fitted_values_new %>%
  filter(day <= max(fitted_values_new$day) - 7) %>%
  droplevels()
tail(fitted_values_new)

if(identical(fitted_values,fitted_values_new)) {
  usethis::ui_done("Nothing to update in fitted_values.")
} else {
  fitted_values <- fitted_values_new
  usethis::use_data(fitted_values,
                    overwrite = TRUE)
  deploy_app <- TRUE
}
#=======================================================================
# Forecast
#=======================================================================
Y_forecast <- MCMCsummary(coda_samples, params = "Y_forecast",
                          probs = c(0.025, 0.05, 0.1, 0.5, 0.90,
                                    0.95, 0.975))

data_forecast <- data.frame("Country" = rep(rownames(country_matrix),
                                            n_ahead),
                            "cases" = Y_forecast$`50%`,
                            "day" = rep(1:n_ahead,
                                        each = nrow(country_matrix)),
                            "Lower10" = Y_forecast$`10%`,
                            "Upper90" = Y_forecast$`90%`,
                            "Lower5" = Y_forecast$`5%`,
                            "Upper95" = Y_forecast$`95%`,
                            "Lower2_5" = Y_forecast$`2.5%`,
                            "Upper97_5" = Y_forecast$`97.5%`)
#-----------------------------------------------------------------------
# Data
#-----------------------------------------------------------------------
work_data2 <- work_data

work_data2$Country <- work_data2$countriesAndTerritories
work_data2$Lower10 <- work_data2$Upper90 <- work_data2$Lower5 <-
  work_data2$Upper95 <- work_data2$Lower2_5 <-
  work_data2$Upper97_5 <- NA
work_data2$Indicator <- 0
data_forecast$Indicator <- 1
data_forecast$time <- as.numeric(as.character(data_forecast$day)) +
  max(work_data2$time)
head(work_data2)
#-----------------------------------------------------------------------
colnames(work_data2)
load("data/shiny_data.rda")

shiny_data_new <- work_data2[, c("Country", "cases", "time", "Indicator",
                             "Lower10", "Upper90", "Lower5", "Upper95",
                             "Lower2_5", "Upper97_5")]
shiny_data_new <- rbind(
  shiny_data_new,
  data_forecast[, c("Country", "cases", "time", "Indicator",
                    "Lower10", "Upper90", "Lower5", "Upper95",
                    "Lower2_5", "Upper97_5")])
head(shiny_data_new)
tail(shiny_data_new)

if(identical(shiny_data_new, shiny_data)) {
  usethis::ui_done("Nothing to update in shiny_data.")
} else {
  shiny_data <- shiny_data_new
  usethis::use_data(shiny_data, overwrite = TRUE)
  deploy_app <- TRUE
}
#-----------------------------------------------------------------------
# Cluster
#-----------------------------------------------------------------------
fitted_values$country <- as.factor(fitted_values$country)
fitted_values$country <- as.factor(gsub(pattern = "_", replacement = " ",
                                        fitted_values$country))

load("data/last_60.rda")
load("data/tsdist.rda")

last_60_new <- fitted_values %>%
  dplyr::filter(as.numeric(day) -
                  max(as.numeric(fitted_values$day)) + 61 > 0) %>%
  dplyr::select(day, country, ar) %>%
  pivot_wider(id_cols = 1:2,
              names_from = "country",
              values_from = "ar") %>%
  dplyr::select(-day)
tsdist_new <- diss(t(last_60_new), "DTWARP")
names(tsdist_new) <- colnames(last_60_new)

if(identical(last_60, last_60_new)){
  usethis::ui_done("Nothing to update in last_60")
} else{
  usethis::ui_todo("Saving last_60...")
  last_60 <- last_60_new
  use_data(last_60, overwrite = TRUE)
  deploy_app <- TRUE
}

if(identical(tsdist_new, tsdist)){
  usethis::ui_done("Nothing to update in tsdist")
} else{
  usethis::ui_todo("Saving tsdist...")
  tsdist <- tsdist_new
  use_data(tsdist, overwrite = TRUE)
  deploy_app <- TRUE
}

use_data(tsdist, overwrite = TRUE)
#-----------------------------------------------------------------------
