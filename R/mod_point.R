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
 
  )
}
    
#' point Server Function
#'
#' @noRd 
mod_point_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_point_ui("point_ui_1")
    
## To be copied in the server
# callModule(mod_point_server, "point_ui_1")
 
