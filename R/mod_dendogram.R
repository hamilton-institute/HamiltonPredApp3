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
 
  )
}
    
#' dendogram Server Function
#'
#' @noRd 
mod_dendogram_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_dendogram_ui("dendogram_ui_1")
    
## To be copied in the server
# callModule(mod_dendogram_server, "dendogram_ui_1")
 
