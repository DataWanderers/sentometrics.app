
howTime_ui <- function(id) {
  ns <- NS(id)
  tags$table(
    id = "inputs-table",
    style = "margin-bottom: 0px; width: 100%",
    tags$tr(
      tags$td(
        style = "margin-bottom: 0px",
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
    lag = 1
  )

  output$selectHowUI <- renderUI({
    output <- tagList()
    output[[1]] <- selectInput(
      inputId = ns("selectHow"),
      label = "Select across-time aggregation",
      choices = myvals$choices,
      selected = "equal_weight",
      multiple = TRUE
    )
    output[[2]] <- column(6, style = "padding:0px;", selectInput(
      inputId = ns("selectFrequency"),
      label = "By?",
      choices = c("day", "week", "month", "year"),
      selected = "month",
      multiple = FALSE
    ))
    output[[3]] <- column(6, numericInput(
      inputId = ns("selectLag"),
      label = "Lag?",
      min = 1,
      value = 14
    ))
    output
  })

  observeEvent(input$selectHow, {
    myvals$selected <- input$selectHow
  })

  observeEvent(input$selectFrequency, {
    myvals$by <- input$selectFrequency
  })
  
  observeEvent(input$selectLag, {
    myvals$lag <- input$selectLag
  })
  
  
  return(myvals)
}

