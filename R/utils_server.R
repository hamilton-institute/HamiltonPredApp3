`%$%` <- magrittr::`%$%`

`%||%` <- purrr::`%||%`

empty_to_na <- function(x) {
  ifelse(shiny::isTruthy(x), x, NA)
}

update_message <- function(x) {
  if (x < 0) {
    message <- fontawesome::fa(name = "arrow-down", fill = "blue")
  } else if (x == 0) {
    message <- fontawesome::fa(name = "arrow-right", fill = "black")
  }else if (x > 0) {
    message <- fontawesome::fa(name = "arrow-up", fill = "red")
  }
  return(message)
}

create_description <- function(text, title = "Description"){
  shinydashboardPlus::boxPlus(
    width = NULL,
    status = "warning",
    closable = TRUE,
    collapsible = TRUE,
    tags$div(
      style="text-align:center",
      tags$hr(style="border-color: black;"),
      tags$h3(title),
      tags$hr(style="border-color: black;"),
    ),
    tags$div(
      style="text-align:justified;color: black",
      tags$p(text, style = "font-size:20px;", align = "justify")
    )
  )
}

get_description_forecast <- function(){
  HTML(paste0("<p>Our model displayed an excellent predictive performance for ",
  "short-term forecasting, especially for the first six days ahead.</p>",
  "<p>The modelling framework allows for forecasting the daily number of new ",
  "COVID-19 cases for each country and territory for which data has been gathered ",
  "by the ", tags$b("European Centre for Disease Prevention and Control", style = paste0("color: ",hamiltonThemes:::distill_status_to_colour("primary"))), "(ECDC). ",
  "It introduces statistical novelty in terms of modelling the autoregressive ",
  "parameter as a function of time. This makes it highly flexible to adapt to ",
  "changes in the autoregressive structure of the data over time. In the COVID-19 ",
  "pandemic, this translates directly in improved predictive power in terms of ",
  "forecasting future number of daily cases. Our objective here is to provide a ",
  "simple, yet not overly simplistic, framework for country-level decision-making, ",
  "and we understand this might be easier for smaller countries when compared to ",
  "nations of continental dimensions, where state-level decision-making should be ",
  "more effective.</p><p>To forecast future observations, we use the median value ",
  "rather than the mean value,  and this is reasonable for short-term forecasting, ",
  "since the error accumulates from one time step to the other.</p>"
  ))
}

get_description_ar <- function() {
  paste0("The autoregressive component in the model has a direct relationship ",
  "with the pandemic behaviour over time for each country. It is directly propor",
  "tional to the natural logarithm of the daily number of cases, given what happ",
  "ened in the previous day. Therefore, it is sensitive to changes and can be ",
  "helpful detecting possible waves.")
}

get_description_dendogram <- function(){
  paste0("Dendrogram representing the hierarchical clustering of countries based ",
  "on their estimated autoregressive parameters. The clustering used Ward's method ",
  "and pairwise dynamic time warp distances between the countries' time series. ",
  "Each of 10 clusters is represented with a different colour. Country abbreviations: ",
  "BSES = Bonaire, Saint Eustatius and Saba; IC Japan = Cases on an international ",
  "conveyance - Japan; CAE = Central African Republic; DRC = Democratic Republic ",
  "of the Congo; NMI = Northern Mariana Islands; SKN = Saint Kitts and Nevis; SVG ",
  "= Saint Vincent and the Grenadines; STP = São Tomé and Príncipe; TC Islands = ",
  "Turks and Caicos Islands; UAE = United Arab Emirates.")
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}