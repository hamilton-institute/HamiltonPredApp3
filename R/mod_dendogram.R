#' dendogram UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dendogram_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::bs4Card(
        title = "",
        closable = FALSE,
        collapsible = FALSE,
        width = 12,
        create_description(get_description_dendogram()),
        hr(),
        fluidRow(
          column(
            width = 12,
            fluidRow(
              column(
                width = 2,
                wellPanel(
                  selectInput(
                    inputId = ns("dendro_country"),
                    label = "Country Query:",
                    choices = levels(fitted_values$country),
                    selected = NULL,
                    multiple = TRUE
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  selectInput(
                    inputId = ns("dendro_type"),
                    label = "Type of tree to be drawn:",
                    choices = c("phylogram", "cladogram", "fan",
                                "radial"),
                    selected = "cladogram",
                    multiple = FALSE
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = ns("tree_col"),
                    label = "Number of Clusters:",
                    value = 10,
                    min = 1,
                    max = 30
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = ns("dendro_cex"),
                    label = "Character Expansion:",
                    value = 1.2,
                    min = 0,
                    max = 6
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = ns("dendro_edge"),
                    label = "Width of the branches:",
                    value = 1,
                    min = 0,
                    max = 6
                  )
                )
              ),
              #---------------------------------------------------------
              column(
                width = 2,
                wellPanel(
                  numericInput(
                    inputId = ns("dendro_height"),
                    label = "Height in px:",
                    value = 3000,
                    min = 250,
                    max = 8000
                  )
                )
              )
              #---------------------------------------------------------
            ),
              uiOutput(ns("plot_dend_ui"))
            )
          )
        )
      )
    )
}
    
#' dendogram Server Function
#'
#' @noRd 
mod_dendogram_server <- function(input, output, session){
  ns <- session$ns
 
  output$plot_dend_ui <- renderUI({
    plotOutput(ns("dend"),  height = paste0(input$dendro_height, "px"))
  })
  #---------------------------------------------------------------------
  output$dend <- renderPlot({
    cols <- gg_color_hue(input$tree_col)
    if (input$tree_col == 10){
      cols5 <- cols[5]
      cols[5] <- cols[2]
      cols[2] <- cols5
    }
    hc <- hclust(tsdist, "ward.D2")
    
    clus10 <- stats::cutree(hc, input$tree_col)
    
    country_query <- input$dendro_country
    country_fonts <- rep(1, nrow(tsdist))
    country_fonts[names(last_60) %in% country_query] <- 2
    country_cex <- rep(input$dendro_cex, nrow(tsdist))
    country_cex[names(last_60) %in% country_query] <-
      input$dendro_cex + 0.25 * input$dendro_cex
    plot(ape::as.phylo(hc), type = input$dendro_type, tip.color = cols[clus10],
         cex = country_cex, font = country_fonts, edge.width =
           input$dendro_edge,
         use.edge.length = TRUE)
  })
  #---------------------------------------------------------------------
  observeEvent(input$dendro_type, {
    if (input$dendro_type == "fan") {
      updateNumericInput(
        session,
        inputId = ns("dendro_height"),
        label = "Height in px:",
        value = 1000,
        min = 250,
        max = 8000
      )
    }
    if (input$dendro_type == "radial") {
      updateNumericInput(
        session,
        inputId = ns("dendro_height"),
        label = "Height in px:",
        value = 1500,
        min = 250,
        max = 8000
      )
    }
  }
  )
  
}
    
## To be copied in the UI
# mod_dendogram_ui("dendogram_ui_1")
    
## To be copied in the server
# callModule(mod_dendogram_server, "dendogram_ui_1")
 
