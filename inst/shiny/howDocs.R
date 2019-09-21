
howDocs_ui <- function(id) {
  ns <- NS(id)
  tags$table(
    id = "inputs-table",
    style = "width: 100%",
    tags$tr(
      tags$td(
        tags$h4(
          style = "align-text: center",
          "Across-document aggregation"
        )
      )
    ),
    tags$tr(
      tags$td(
        uiOutput(ns("selectHowUI"))
      )
    )
  )
}

howDocs_server <- function(input, output, session) {
  ns <- session$ns

  myvals <- reactiveValues(
    selected = NULL,
    choices = sentometrics::get_hows()$docs
  )

  output$selectHowUI <- renderUI({
    selectInput(
      inputId = ns("selectHow"),
      label = "Select across-document aggregation",
      choices = myvals$choices,
      selected = myvals$selected,
      multiple = FALSE
    )
  })

  observeEvent(input$selectHow,{
    myvals$selected <- input$selectHow
  })

  return(myvals)
}

