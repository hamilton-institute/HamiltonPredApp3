args <- commandArgs(trailingOnly = TRUE)

deploy_app <- FALSE

usethis::ui_todo("Loading packages...")

suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(rjags)
  library(coda)
  library(bayesplot)
  library(MCMCvis)
  library(runjags)
  library(DescTools)
  library(magrittr)
  library(gridExtra)
  library(corrplot)
  library(bayestestR)
  library(dplyr)
  library(TSclust)
  library(xtable)
  
})

usethis::ui_todo("Downloading data...")

source("data-raw/model.R")

usethis::ui_todo("Saving shiny_data...")
source("data-raw/shiny_data.R")

usethis::ui_todo("Saving work_data...")
source("data-raw/work_data.R")

usethis::ui_todo("Saving fitted_values...")
source("data-raw/fitted_values.R")

if (deploy_app) {
  
  remotes::install_deps(
    dependencies = TRUE,
    upgrade = "never",
    repos = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest"
  )
  
  rsconnect::setAccountInfo(
    name = 'apmuhamilton',
    token = args[1],
    secret= args[2]
  )
  
  files <- list.files('.')
  files <- files[files != 'data-raw']
  
  rsconnect::deployApp(
    appFiles = files,
    appName = 'hamiltonPredApp3',
    forceUpdate = TRUE,
    account = 'apmuhamilton'
  )
  
} else {
  message("Nothing to deploy.")
}
