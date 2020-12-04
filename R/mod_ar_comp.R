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
 
  )
}
    
#' ar_comp Server Function
#'
#' @noRd 
mod_ar_comp_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_ar_comp_ui("ar_comp_ui_1")
    
## To be copied in the server
# callModule(mod_ar_comp_server, "ar_comp_ui_1")
 
