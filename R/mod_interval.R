#' interval UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_interval_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::bs4TabCard(
        title = "",
        id = "tabcard",
        closable = FALSE,
        collapsible = FALSE,
        width = 12,
        bs4Dash::bs4TabPanel(
          tabName = "Graphical Output",
          fluidPage(
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
                    width = 3,
                    selectInput(
                      inputId = ns("countryC"),
                      label = "Country:",
                      choices = levels(shiny_data$Country),
                      selected = c("Ireland", "United Kingdom"),
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    sliderInput(ns("time2"),
                                "Dates:",
                                min = min(shiny_data$time),
                                max = max(shiny_data$time),
                                value = c(max(shiny_data$time) - 20,
                                          max(shiny_data$time))
                    )
                  ),
                  column(
                    width = 2,
                    selectInput(
                      inputId = ns("cred_int"),
                      label = "Credible Interval:",
                      choices = c("80%", "90%", "95%"),
                      selected = "80%",
                      multiple = FALSE
                    )
                  ),
                  column(
                    width = 1,
                    numericInput(
                      inputId = ns("int_cols"),
                      label = "Columns:",
                      value = 2,
                      min = 1,
                      max = 7
                    )
                  ),
                  column(
                    width = 1,
                    numericInput(
                      inputId = ns("forecast_height"),
                      label = "Height in px:",
                      value = 300,
                      min = 250,
                      max = 8000
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    uiOutput(ns("forecast_int_ui")),
                    checkboxInput(
                      inputId = ns("log2"),
                      label = "Log Scale (base 10)",
                      value = FALSE)
                  )
                  
                )
              ),
              "Reported Cases by Country, Territory, or Conveyance. Users are advised to use all data and forecast with caution and awareness of their limitations.",
            ),
            #-----------------------------------------------------------
            shinydashboardPlus::gradientBox(
              width = 12,
              icon = "fa fa-th",
              title = "Forecast - Tomorrow",
              boxToolSize = "lg",
              footer = column(
                width = 12,
                align = "center",
                fluidRow(
                  column(
                    width = 3,
                    bs4Dash::valueBoxOutput(ns("IrelandC1Box"), width = NULL) %>%
                      hamiltonThemes::distill_load_spinner()
                  ),
                  column(
                    width = 3,
                    bs4Dash::valueBoxOutput(ns("IrelandC2Box"), width = NULL) %>%
                      hamiltonThemes::distill_load_spinner()
                  ),
                  column(
                    width = 3,
                    bs4Dash::valueBoxOutput(ns("IrelandC3Box"), width = NULL) %>%
                      hamiltonThemes::distill_load_spinner()
                  ),
                  column(
                    width = 3,
                    bs4Dash::valueBoxOutput(ns("IrelandC4Box"), width = NULL) %>%
                      hamiltonThemes::distill_load_spinner()
                  ),
                  column(
                    width = 3,
                    bs4Dash::valueBoxOutput(ns("IrelandC5Box"), width = NULL) %>%
                      hamiltonThemes::distill_load_spinner()
                  ),
                  column(
                    width = 3,
                    bs4Dash::valueBoxOutput(ns("IrelandC6Box"), width = NULL) %>%
                      hamiltonThemes::distill_load_spinner()
                  ),
                  column(
                    width = 3,
                    bs4Dash::valueBoxOutput(ns("IrelandC7Box"), width = NULL) %>%
                      hamiltonThemes::distill_load_spinner()
                  ),
                  column(
                    width = 3,
                    bs4Dash::valueBoxOutput(ns("IrelandC8Box"), width = NULL) %>%
                      hamiltonThemes::distill_load_spinner()
                  )
                )
              )
            )
            #-----------------------------------------------------------
          )
        ),
        bs4Dash::bs4TabPanel(
          tabName = "General Description",
          fluidPage(
            column(
              width = 12,
              create_description(get_description_forecast())
            )
          )
        )
      )

    )
  )
}
    
#' interval Server Function
#'
#' @noRd 
mod_interval_server <- function(input, output, session){
  ns <- session$ns
 
  output$forecast_int_ui <- renderUI({
    plotly::plotlyOutput(ns("forecast_int"),
                 height = paste0(input$forecast_height, "px"),
                 width = "90%")
  })
  
  forecast_data_int2 <- reactive({
    if (input$log2 == FALSE) {
      shiny_data %>%
        dplyr::filter(Country %in% input$countryC) %>%
        dplyr::filter(time >= input$time2[1]) %>%
        dplyr::filter(time <= input$time2[2]) %>%
        dplyr::mutate_at(5:10, dplyr::funs(round(., 0)))%>%
        droplevels()
    }else {
      log_f <- function(x) log(x + 1)
      shiny_data$cases[shiny_data$cases < 0] <- NA
      shiny_data %>%
        dplyr::filter(Country %in% input$countryC) %>%
        dplyr::filter(time >= input$time2[1]) %>%
        dplyr::filter(time <= input$time2[2]) %>%
        dplyr::mutate_at(5:10, dplyr::funs(round(., 0)))%>%
        dplyr::mutate_at(c(2, 5:10), dplyr::funs(log_f)) %>%
        dplyr::mutate_at(c(2, 5:10), dplyr::funs(round(., 3))) %>%
        droplevels()
    }
  })
  
  extra_points_2 <- reactive({
    if (input$log2 == FALSE) {
      work_data %>%
        dplyr::filter(Country %in% input$countryC)
    }else {
      log_f <- function(x) log(x + 1)
      work_data$cases[work_data$cases < 0] <- NA
      work_data %>%
        dplyr::filter(Country %in% input$countryC) %>%
        mutate_at(5, funs(log_f)) %>%
        mutate_at(5, funs(round(., 3))) %>%
        droplevels()
    }
  })
  
  output$forecast_int <- plotly::renderPlotly({
    h <- input$forecast_height
    if (input$cred_int == "80%") {
      a <-  "Lower10"
      b <- 'Upper90'
    }
    if (input$cred_int == "90%"){
      a <- "Lower5"
      b <- "Upper95"
    } 
    if (input$cred_int == "95%"){
      a <- "Lower2_5"
      b <- "Upper97_5"
    } 
    w1 <- forecast_data_int2() %>%
      ggplot2::ggplot(
        ggplot2::aes(
          y = cases,
          x = time,
          group = Country,
          text = paste("Date: ", format(time, "%Y-%m-%d"),
                                  "<br>", Country, ": ", cases,
                                  "[",.data[[a]], ", ", .data[[b]], "]",
                                  "<br> Type: ", Indicator))) +
      ggplot2::geom_ribbon(ggplot2::aes_string(ymin = a, ymax = b), fill = "gray",
                  alpha=0.6,  col = NA) +
      ggplot2::geom_point(ggplot2::aes(shape = Indicator,  colour = Indicator), cex = 0.9,  alpha = 0.9,
                 stroke = 0.5) +
      ggplot2::scale_colour_manual(values = c("black", "blue"),  name = "Type: ") +
      ggplot2::geom_line(size = 0.3) +
      ggplot2::facet_wrap(~Country,  scales = "free", ncol = input$int_cols) +
      ggplot2::theme_bw(base_size = 14) +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = hamiltonThemes:::distill_status_to_colour("primary")),
        strip.text = ggplot2::element_text(color = "white", size = 11)
      )
      ggplot2::xlab("Time") +
      ggplot2::geom_vline(xintercept = max(shiny_data$time) - 6,
                 color = "gray", alpha = 0.7,  linetype = 2)
    if (input$log2 == FALSE) {
      w2 <- w1 + ggplot2::ylab("Number of Cases")
    }else {
      w2 <- w1 + ggplot2::ylab("log(number of cases + 1)")
    }
    if (input$time2[2] < min(extra_points_2()$time)) {
      w3 <- w2
    }else {
      w3 <- w2 +
        ggplot2::geom_point(data = extra_points_2(),
                            ggplot2::aes(y = cases, x = time, group = Country,
                       shape = Indicator, colour = Indicator),
                   alpha = 0.7,  stroke = 0.5,
                   cex = 0.9) +
        ggplot2::scale_shape_manual(values = c(1, 2), name="Type: ")
    }
    w4 <- w3 +
      ggplot2::theme(legend.position = "rigth")
    plotly::ggplotly(w4, dynamicTicks = TRUE, tooltip = c("text"))
  })

  today <- format(Sys.time(), "%Y-%m-%d")
  today <- as.Date(today) + 1
  total <- 8
  if (today > max(shiny_data$time)) today <- max(shiny_data$time)
  #---------------------------------------------------------------------
  date_box <- reactive({
    if (length(input$countryC) <= total) {
      shiny_data %>%
        dplyr::filter(time == today) %>%
        dplyr::filter(Country %in% input$countryC)
    }else {
      shiny_data %>%
        dplyr::filter(time == today) %>%
        dplyr::filter(Country %in% input$countryC[1:total])
    }
  })
  #---------------------------------------------------------------------
  seed_sel <- round(runif(1, 0, 10^6), 0)
  sample_box <- reactive({
    country_sel <- shiny_data$Country
    country_sel <- country_sel[!country_sel %in% input$countryC]
    sample_sel <- shiny_data %>%
      dplyr::filter(time == today) %>%
      dplyr::filter(Country %in% country_sel)
    sample_sel[order(sample_sel$cases,  decreasing = TRUE), ]
  })
  #---------------------------------------------------------------------
  output$IrelandC1Box <- bs4Dash::renderValueBox({
    index <- 1
    if (is.na(date_box()[index,1]) == FALSE) {
      #-----------------------------------------------------------------
      s0 <- date_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(date_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }else {
      s0 <- sample_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(sample_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }
  })
  #---------------------------------------------------------------------
  output$IrelandC2Box <- bs4Dash::renderValueBox({
    index <- 2
    if (is.na(date_box()[index,1]) == FALSE) {
      #-----------------------------------------------------------------
      s0 <- date_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(date_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }else {
      s0 <- sample_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(sample_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }
  })
  #---------------------------------------------------------------------
  output$IrelandC3Box <- bs4Dash::renderValueBox({
    index <- 3
    if (is.na(date_box()[index,1]) == FALSE) {
      #-----------------------------------------------------------------
      s0 <- date_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(date_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }else {
      s0 <- sample_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(sample_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }
  })
  #---------------------------------------------------------------------
  output$IrelandC4Box <- bs4Dash::renderbs4ValueBox({
    index <- 4
    if (is.na(date_box()[index,1]) == FALSE) {
      #-----------------------------------------------------------------
      s0 <- date_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(date_box()[index, "Country"],
                              style =  paste0("font-size: 2vmax; margin-bottom: 0;")),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    } else {
      s0 <- sample_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(sample_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
                        subtitle = tags$p(HTML(text)),
                        icon = "far fa-flag",
                        status = "primary")
    }
  })
  #---------------------------------------------------------------------
  output$IrelandC5Box <- bs4Dash::renderValueBox({
    index <- 5
    if (is.na(date_box()[index,1]) == FALSE) {
      #-----------------------------------------------------------------
      s0 <- date_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(date_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }else {
      s0 <- sample_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(sample_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }
  })
  #---------------------------------------------------------------------
  output$IrelandC6Box <- bs4Dash::renderValueBox({
    index <- 6
    if (is.na(date_box()[index,1]) == FALSE) {
      #-----------------------------------------------------------------
      s0 <- date_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(date_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }else {
      s0 <- sample_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(sample_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }
  })
  #---------------------------------------------------------------------
  output$IrelandC7Box <- bs4Dash::renderValueBox({
    index <- 7
    if (is.na(date_box()[index,1]) == FALSE) {
      #-----------------------------------------------------------------
      s0 <- date_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(date_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }else {
      s0 <- sample_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(sample_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }
  })
  #---------------------------------------------------------------------
  output$IrelandC8Box <- bs4Dash::renderValueBox({
    index <- 8
    if (is.na(date_box()[index,1]) == FALSE) {
      #-----------------------------------------------------------------
      s0 <- date_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(date_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }else {
      s0 <- sample_box()[index, "cases"]
      upmc0 <- update_message(s0)
      text = paste0("Number of Cases: ",' ', upmc0,' ',
                    format(s0, big.mark=','), br(),
                    "Date: ", format(today, "%d-%m-%Y"))
      #-----------------------------------------------------------------
      bs4Dash::valueBox(value = tags$p(sample_box()[index, "Country"],
                                       style =  "font-size: 2vmax; margin-bottom: 0;"),
               subtitle = tags$p(HTML(text)),
               icon = "far fa-flag",
               status = "primary")
    }
  })
 
}
    
## To be copied in the UI
# mod_interval_ui("interval_ui_1")
    
## To be copied in the server
# callModule(mod_interval_server, "interval_ui_1")
 
