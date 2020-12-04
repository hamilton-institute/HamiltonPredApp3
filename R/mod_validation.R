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
 
  )
}
    
#' validation Server Function
#'
#' @noRd 
mod_validation_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_validation_ui("validation_ui_1")
    
## To be copied in the server
# callModule(mod_validation_server, "validation_ui_1")
 
