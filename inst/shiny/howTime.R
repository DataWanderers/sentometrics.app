
howTime_ui <- function(id) {
  ns <- NS(id)
  tags$table(
    id = "inputs-table",
    style = "width: 100%",
    tags$tr(
      tags$td(
        tags$h4(
          style = "align-text: center",
          "Across-time aggregation"
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

howTime_server <- function(input, output, session) {
  ns <- session$ns

  myvals <- reactiveValues(
    selected = NULL,
    choices = sentometrics::get_hows()$time[!(sentometrics::get_hows()$time %in% "own")],
    by = NULL,
    lag = 0
  )

  output$selectHowUI <- renderUI({
    selectInput(
      inputId = ns("selectHow"),
      label = "Select across-time aggregation",
      choices = myvals$choices,
      selected = "equal_weight",
      multiple = TRUE
    )
    selectInput(
      inputId = ns("selectFrequency"),
      label = "By?",
      choices = c("day", "week", "month", "year"),
      selected = "month",
      multiple = FALSE
    )
  })

  observeEvent(input$selectHow,{
    myvals$selected <- input$selectHow
  })

  return(myvals)
}

