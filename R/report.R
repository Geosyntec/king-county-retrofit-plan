report_UI <- function(id) {
  ns <- NS(id)
  tagList()
}

report_server <- function(id, final_tables) {
  moduleServer(
    id,
    function(input, output, session) {
      #  observe(print('ok'))%>% bindEvent(final_tables())
      # observe({
      #     req(final_tables())
      #     print('final tables')
      # }
      #     )
    }
  )
}
