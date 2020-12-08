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

  callModule(
    mod_point_server, 
    "point_ui_1"
  )
  
  callModule(
    mod_table_server, 
    "table_ui_1"
  )

  callModule(
    mod_ar_comp_server,
    "ar_comp_ui_1"
  )
  
  callModule(
    mod_dendogram_server,
    "dendogram_ui_1"
  )
  
  callModule(
    mod_validation_server, 
    "validation_ui_1"
  )
  
}
