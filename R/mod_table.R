#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::bs4Card(
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
                width = 5,
                selectInput(
                  inputId = ns("countryB"),
                  label = "Country:",
                  choices = levels(shiny_data$Country),
                  selected = c("Brazil", "United States of America"),
                  multiple = TRUE
                )
              )
            )
          ),
          "Reported Cases by Country, Territory, or Conveyance. Users are advised to use all data and forecast with caution and awareness of their limitations."
        )
      ),
      fluidRow(
        column(
          width = 12,
          DT::dataTableOutput(ns("summary_table")),
        )
      )
    )
  )
}
    
#' table Server Function
#'
#' @noRd 
mod_table_server <- function(input, output, session){
  ns <- session$ns
 
  myContainer <- htmltools::withTags(
    table(
      class = 'display',
      thead(
        tr(
          th(),
          th(),
          th(),
          th(colspan = 2, '80% CI', class = "dt-center"),
          th(colspan = 2, '90% CI', class = "dt-center"),
          th(colspan = 2, '95% CI', class = "dt-center")
        ),
        tr(
          lapply(c("Country", "Day", "Cases", "Lower",
                   "Upper",  "Lower",
                   "Upper", "Lower",
                   "Upper"), th)
        )
      )
    )
  )
  #---------------------------------------------------------------------
  forecast_data_table <- reactive({
    shiny_data %>%
      dplyr::filter(Country %in% input$countryB) %>%
      dplyr::filter(Indicator == "Forecast") %>%
      droplevels()
  })
  #---------------------------------------------------------------------
  output$summary_table <- DT::renderDataTable({
    data <- forecast_data_table()
    DT::datatable(data[, c(1, 3, 2, 5:10)], container = myContainer,
                  rownames = FALSE, filter = 'bottom',
                  options = list(lengthMenu = c(7, 14, 50, 100),
                                 pageLength = 7,  dom = "tip",
                                 autoWidth = TRUE,
                                 columnDefs = list(
                                   list(
                                     searchable = FALSE,
                                     targets = c(2:8))))) %>%
      DT::formatRound(c(3:9), 2)
  })
}
    
## To be copied in the UI
# mod_table_ui("table_ui_1")
    
## To be copied in the server
# callModule(mod_table_server, "table_ui_1")
 
