#' preference_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_preference_input_ui <- function(id){
  ns <- NS(id)
  #table with preferences
  tagList(
  )
}

#' preference_input Server Functions
#'
#' @noRd
mod_preference_input_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_preference_input_ui("preference_input_1")

## To be copied in the server
# mod_preference_input_server("preference_input_1")
