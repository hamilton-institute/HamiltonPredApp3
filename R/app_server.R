#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @noRd
app_server <- function( input, output, session ) {

  
  callModule(
    mod_interval_server, 
    "interval_ui_1"
  )

  
}