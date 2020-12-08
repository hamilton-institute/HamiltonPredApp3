#' ar_comp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ar_comp_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::bs4TabCard(
        title = "",
        id = "tabca",
        closable = FALSE,
        collapsible = FALSE,
        width = 12,
        bs4Dash::bs4TabPanel(
          tabName = "Graphical Output",
          fluidPage(
            shinydashboardPlus::gradientBox(
              width = 12,
              icon = "fa fa-th",
              title = "Autoregressive Component with 95% credible interval - COVID-19 Coronavirus Pandemic",
              boxToolSize = "lg",
              footer = column(
                width = 12,
                align = "center",
                fluidRow(
                  column(
                    width = 3,
                    selectInput(
                      inputId = ns("country_ar"),
                      label = "Country:",
                      choices = levels(fitted_values$country),
                      selected = c("China"),
                      multiple = TRUE
                    )
                  ),
                  column(
                    width = 3,
                    sliderInput(ns("time_ar"),
                                "Dates:",
                                min = min(fitted_values$day),
                                max = max(fitted_values$day),
                                value = c(min(fitted_values$day),
                                          max(fitted_values$day))
                    )
                  ),
                  column(
                    width = 1,
                    selectInput(
                      inputId = ns("dates_ar"),
                      label = "Date Breaks:",
                      choices = c("day", "week", "2 weeks", "3 weeks", "month",
                                  "2 months", "3 months", "6 months"),
                      selected = c("month"),
                      multiple = FALSE
                    )
                  ),
                  column(
                    width = 1,
                    selectInput(
                      inputId = ns("ar_scale"),
                      label = "Y-Scale:",
                      choices = c("fixed", "free",  "free_y"),
                      selected = c("fixed"),
                      multiple = FALSE
                    )
                  ),
                  column(
                    width = 1,
                    numericInput(
                      inputId = ns("ar_cols"),
                      label = "Columns:",
                      value = 2,
                      min = 1,
                      max = 7
                    )
                  ),
                  column(
                    width = 1,
                    numericInput(
                      inputId = ns("height_ar"),
                      label = "Height in px:",
                      value = 450,
                      min = 250,
                      max = 8000
                    )
                  )
                ),
                uiOutput(ns("ar_ui"))
              ),
              "Reported Cases by Country, Territory, or Conveyance. Users are advised to use all data and,  consequently, results with caution and awareness of their limitations."
            )
          )
        ),
        bs4Dash::bs4TabPanel(
          tabName = "General Description",
          fluidPage(
            column(
              width = 12,
              create_description(get_description_ar())
            )
          )
        )
      )
    )
  )
}
    
#' ar_comp Server Function
#'
#' @noRd 
mod_ar_comp_server <- function(input, output, session){
  ns <- session$ns
 
  output$ar_ui <- renderUI({
    plotly::plotlyOutput(ns("ar_plot"),
                 height = paste0(input$height_ar, "px"),
                 width = "70%")
  })
  #---------------------------------------------------------------------
  ar_data <- reactive({
    fitted_values %>%
      dplyr::filter(country %in% input$country_ar) %>%
      dplyr::filter(day >= input$time_ar[1]) %>%
      dplyr::filter(day <= input$time_ar[2]) %>%
      droplevels()
  })
  #---------------------------------------------------------------------
  output$ar_plot <- plotly::renderPlotly({
    h <- input$height_ar
    w1 <- ggplot2::ggplot(data = ar_data(),
                          ggplot2::aes(x = day, y = ar, group = country,
                     text = paste("Date: ", format(day, "%Y-%m-%d"),
                                  "<br>", country, ": ", ar))) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = hamiltonThemes:::distill_status_to_colour("primary")),
        strip.text = ggplot2::element_text(color = "white", size = 11)
      ) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ar_low, ymax = ar_upp), alpha = .5) +
      ggplot2::geom_abline(intercept = 0, slope = 0, lty = 2, lwd = .25) +
      ggplot2::facet_wrap(~ country,  scales = input$ar_scale, ncol = input$ar_cols) +
      ggplot2::xlab("Time") +
      ggplot2::ylab("Autoregressive Component")
    
    if (input$dates_ar == "day" || input$dates_ar == "week" ||
        input$dates_ar == "2 weeks" || input$dates_ar == "3 weeks") {
      w2 <- w1 +
        ggplot2::scale_x_date(breaks = seq(min(ar_data()$day),
                                  max(ar_data()$day),
                                  by=input$dates_ar),
                     date_labels = "%y-%b-%d")
    }else {
      w2 <- w1 +
        ggplot2::scale_x_date(breaks = seq(min(ar_data()$day),
                                  max(ar_data()$day),
                                  by=input$dates_ar), date_labels = "%y-%b")
    }
    
    plotly::ggplotly(w2, dynamicTicks = FALSE, tooltip = c("text")) %>%
      plotly::layout(legend = list(orientation = "h"))
  })
 
}
    
## To be copied in the UI
# mod_ar_comp_ui("ar_comp_ui_1")
    
## To be copied in the server
# callModule(mod_ar_comp_server, "ar_comp_ui_1")
 
