#' validation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_validation_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    fluidRow(
      bs4Dash::bs4Card(
        title = "",
        id = "tabcard",
        closable = FALSE,
        collapsible = FALSE,
        width = 12,
        uiOutput(ns("text_validation")),
        fluidRow(
          column(
            width = 4,
            wellPanel(
              selectInput(
                inputId = ns("country_validation"),
                label = "Country Query:",
                choices = levels(data_forecast$country),
                selected = NULL,
                multiple = TRUE
              )
            )
          ),
          #---------------------------------------------------------
          column(
            width = 2,
            wellPanel(
              numericInput(
                inputId = ns("vali_cex"),
                label = "Character Expansion:",
                value = 4,
                min = 0,
                max = 8
              )
            )
          ),
          #---------------------------------------------------------
          column(
            width = 2,
            wellPanel(
              numericInput(
                inputId = ns("vali_nudge"),
                label = "Horizontal adjustment:",
                value = 0.6,
                min = 0,
                max = 4
              )
            )
          ),
          #---------------------------------------------------------
          column(
            width = 2,
            wellPanel(
              numericInput(
                inputId = ns("valid_height"),
                label = "Height in px:",
                value = 2800,
                min = 250,
                max = 8000
              )
            )
          )
                #---------------------------------------------------------
        ),
        uiOutput(ns("plot_valid_ui"))
      ),
      bs4Dash::bs4Card(
        title = "",
        id = "tabcard",
        closable = FALSE,
        collapsible = FALSE,
        width = 12,
        uiOutput(ns("text_validation2")),
        plotly::plotlyOutput(ns("ccc_validation"),  height = "400px"),
        uiOutput(ns("text_validation3"))
      )
    )
  )
      
}
    
#' validation Server Function
#'
#' @noRd 
mod_validation_server <- function(input, output, session){
  ns <- session$ns
 
  output$text_validation <- renderUI({
    create_description(
      withMathJax(
        helpText('Logarithm of the observed $y_{it}$ versus the forecasted daily number of cases $y^*_{it}$ for each country, for up to seven days ahead, where each day ahead constitutes one panel. The forecasts were obtained from the autoregressive state-space hierarchical negative binomial model, fitted using data up to 6-May-2020, up to 29-Apr-2020, up to 13-May-2020 for the fisrt,  second,  and third forecast validation process. As an example,  the first day ahead of the third validation corresponds to 14-May-2020, and the seventh to 20-May-2020. Each dot represents a country. The results are very similar, with a concordance correlation between observed and forecasted values greaterthan 0.75 for up to five days ahead.')
      )
    )
  })
  
  output$valid <- renderPlot({
    h <- input$valid_height
    country_list <- input$country_validation
    data_forecast %>%
      ggplot2::ggplot(ggplot2::aes(y = log(Y_forecast + 1, base = 10),
                 x = log(Y_obs + 1, base = 10),
                 label = country)) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = hamiltonThemes:::distill_status_to_colour("primary")),
        strip.text = ggplot2::element_text(color = "white", size = 11)
      ) +
      ggplot2::geom_point(data = subset(data_forecast, !country %in% country_list),
                 cex = input$vali_cex,  alpha = 0.4,  stroke = 0.5) +
      ggplot2::geom_abline(intercept = 0,  slope = 1, lty = 2, lwd = .55) +
      ggplot2::facet_grid(day2 ~ Type) +
      ggplot2::geom_point(data = subset(data_forecast, country %in%  country_list),
                 colour = "darkblue",  cex = input$vali_cex + 1,
                 pch = 24,  alpha = 1,
                 stroke = 0.2,
                 fill = "#32c6ff") +
      ggrepel::geom_text_repel(data = subset(data_forecast, country %in%  country_list &
                                      data_forecast$Y_forecast >= data_forecast$Y_obs),
                      nudge_x = -input$vali_nudge,
                      min.segment.length = 1,
                      size = 5,
                      segment.size = 0.4) +
      ggrepel::geom_text_repel(data = subset(data_forecast, country %in%  country_list &
                                      data_forecast$Y_forecast < data_forecast$Y_obs),
                      nudge_x = input$vali_nudge,
                      segment.size = 0.4,
                      min.segment.length = 1,
                      size = 5) +
      ggplot2::ylab(expression(log[10](y[it]^"*" + 1))) +
      ggplot2::xlab(expression(log[10](y[it] + 1))) +
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = 14),
            strip.text.y = ggplot2::element_text(size = 14),
            axis.text=ggplot2::element_text(size=14),
            axis.title=ggplot2::element_text(size=18))
  })
  #---------------------------------------------------------------------
  output$plot_valid_ui <- renderUI({
    plotOutput(ns("valid"),  height = paste0(input$valid_height, "px"))
  })
  #---------------------------------------------------------------------
  output$text_validation2 <- renderUI({
    create_description(
      withMathJax(
        helpText('Observed concordance correlation coefficient (CCC), Pearson correlation ($r$), and bias corrector factor ($C_b$) between observed ($y_{it}$) and forecasted ($y^*_{it}$) values for each of the days ahead by validation process. Further details about CCC can be found in Lin (1989).'),
        br(),
        helpText("Lin, L. I. (1989). A Concordance Correlation Coefficient to Evaluate Reproducibility. Biometrics, 45(1):255–268."))
    )
  })
  output$ccc_validation <- plotly::renderPlotly({
    ggplot2::ggplot(data_ccc, ggplot2::aes(y = Value, x = day2,  group = Method,
                         linetype = Method, shape = Method)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::theme_bw() +
      ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = hamiltonThemes:::distill_status_to_colour("primary")),
        strip.text = ggplot2::element_text(color = "white", size = 11)
      ) +
      ggplot2::xlab("Days ahead") +
      ggplot2::ylim(0, 1) +
      ggplot2::scale_x_continuous(breaks = seq(1, 7, 1)) +
      ggplot2::facet_wrap(~Type)
  })
  output$text_validation3 <- renderUI({
    tags$div(
      style="text-align:justified;color: black",
      tags$p(
        withMathJax(
          helpText('Even though performance falls substantially for the seventh day ahead, with the lowest CCC between observed and forecasted values close to $0.25$, there are still many countries for which the forecasted daily number of new cases is very close to the observed one. We also found a concordance correlation between observed and forecasted values greater than $0.75$ for up to five days ahead.')), 
        style = "font-size:20px;", 
        align = "justify"
      )
    )
    
  })
}
    
## To be copied in the UI
# mod_validation_ui("validation_ui_1")
    
## To be copied in the server
# callModule(mod_validation_server, "validation_ui_1")
 
