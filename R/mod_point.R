#' point UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_point_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::bs4Card(
      title = "",
      id = "tabcard",
      closable = FALSE,
      collapsible = FALSE,
      width = 12,
      fluidRow(
        shinydashboardPlus::gradientBox(
          width = 12,
          icon = "fa fa-th",
          title = "7-step-ahead Forecast - COVID-19 Coronavirus Pandemic",
          boxToolSize = "lg",
          footer = column(
            width = 12,
            align = "center",
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = ns("countryA"),
                  label = "Country:",
                  choices = levels(shiny_data$Country),
                  selected = c("Brazil", "United States of America"),
                  multiple = TRUE
                )
              ),
              column(
                width = 4,
                sliderInput(ns("time"),
                            "Dates:",
                            min = min(shiny_data$time),
                            max = max(shiny_data$time),
                            value = c(max(shiny_data$time) - 20,
                                      max(shiny_data$time))
                )
              ),
              column(
                width = 2,
                numericInput(
                  inputId = ns("forecast2_height"),
                  label = "Height in px:",
                  value = 450,
                  min = 250,
                  max = 8000
                )
              ),
              column(
                width = 2,
                numericInput(
                  inputId = ns("forecast2_width"),
                  label = "Width in %:",
                  value = 60,
                  min = 250,
                  max = 8000
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                uiOutput(ns("forecast_ui")),
              ),
              column(
                width = 3,
                checkboxInput(
                  inputId = ns("log"),
                  label = "Log Scale (base 10)",
                  value = FALSE)
              ),
              tags$div(
                tags$p("Lines, circles, and triangles represent countries, observed values, and predicted values, respectively.", style = "font-size:16px", align="justify")
              ),
              "Reported Cases by Country, Territory, or Conveyance. Users are advised to use all data and forecast with caution and awareness of their limitations."
              )
            )
          )
        )
      )
  )
}
      
  #' point Server Function
  #'
  #' @noRd 
  mod_point_server <- function(input, output, session){
    ns <- session$ns
  
    output$forecast_ui <- renderUI({
      plotly::plotlyOutput(ns("forecast"),
                   height = paste0(input$forecast2_height, "px"),
                   width = paste0(input$forecast2_width, "%"))
    })
    
    forecast_data <- reactive({
      if (input$log == FALSE) {
        shiny_data %>%
          dplyr::filter(Country %in% input$countryA) %>%
          dplyr::filter(time >= input$time[1]) %>%
          dplyr::filter(time <= input$time[2]) %>%
          droplevels()
      }else {
        shiny_data$cases[shiny_data$cases < 0] <- NA
        shiny_data$cases <- log(shiny_data$cases + 1, base = 10)
        shiny_data %>%
          dplyr::filter(Country %in% input$countryA) %>%
          dplyr::filter(time >= input$time[1]) %>%
          dplyr::filter(time <= input$time[2]) %>%
          droplevels()
      }
    })
    
    extra_points <- reactive({
      if (input$log == FALSE) {
        work_data %>%
          dplyr::filter(Country %in% input$countryA)
      }else {
        work_data$cases[work_data$cases < 0] <- NA
        work_data$cases <- log(work_data$cases + 1, base = 10)
        work_data %>%
          dplyr::filter(Country %in% input$countryA)
      }
    })
    
    output$forecast <- plotly::renderPlotly({
      h <- input$forecast2_height
      w <- input$forecast2_width
      nc <- length(levels(forecast_data()$Country))
      type <- c("Observed" = 1, "Forecast" = 2)
      data_obs <- forecast_data() %>%
        dplyr::filter(Indicator == "Observed") %>%
        droplevels()
      data_for <- forecast_data() %>%
        dplyr::filter(Indicator == "Forecast") %>%
        droplevels()
      w1 <- ggplot2::ggplot(forecast_data()) +
        ggplot2::geom_point(data = data_obs,
                            ggplot2::aes(y = cases, x = time,
                       colour = Country, linetype = Country,
                       text = paste("Date: ", format(time, "%Y-%m-%d"),
                                    "<br>", Country, ": ", cases,
                                    "<br> Type: ", Indicator)),
                   cex = 0.9, shape = 1, alpha = 0.9, stroke = 0.5) +
        ggplot2::geom_point(data = data_for,
                            ggplot2::aes(y = cases, x = time,
                       colour = Country, linetype = Country,
                       text = paste("Date: ", format(time, "%Y-%m-%d"),
                                    "<br>", Country, ": ", cases,
                                    "<br> Type: ", Indicator)),
                   cex = 1.1, shape = 2, alpha = 0.9, stroke = 0.5)
      if (nrow(extra_points()) != 0) {
        w1.1 <-  w1 +
          ggplot2::geom_point(data = extra_points(),
                              ggplot2::aes(y = cases, x = time, colour = Country,
                         linetype = Country,
                         text = paste("Date: ", format(time, "%Y-%m-%d"),
                                      "<br>", Country, ": ", cases,
                                      "<br> Type: ", Indicator)),
                     alpha = 0.9,  stroke = 0.5, cex = 0.9, shape = 1)
      }else {
        w1.1 <- w1
      }
      w1.2 <- w1.1 +
        ggplot2::geom_line(ggplot2::aes(y = cases, x = time,
                      colour = Country, linetype = Country), size = 0.3) +
        ggplot2::xlab("Time") +
        ggplot2::geom_vline(xintercept = max(shiny_data$time) - 6,
                   color = "gray", alpha = 0.7,  linetype = 2)
      if (input$log == FALSE) {
        w2 <- w1.2 + ggplot2::ylab("Number of Cases")
      }else {
        w2 <- w1.2 + ggplot2::ylab("log(number of cases + 1)")
      }
      if (input$time[2] < min(extra_points()$time)) {
        w3 <- w2
      }else {
        w3 <- w2 +
          ggplot2::theme_bw(base_size = 12) +
          ggplot2::theme(legend.position = "right")
      }
      plotly::ggplotly(w3, dynamicTicks = TRUE, tooltip = c("text")) %>%
        plotly::rangeslider() %>%
        plotly::layout(hovermode = "x", dragmode='pan')
    })
  
}
    
## To be copied in the UI
# mod_point_ui("point_ui_1")
    
## To be copied in the server
# callModule(mod_point_server, "point_ui_1")
 
