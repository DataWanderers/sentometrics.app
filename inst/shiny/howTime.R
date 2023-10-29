
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
    ),
    tags$tr(
      tags$td(
        style = "margin-bottom: 0px",
        uiOutput(ns("selectHowUIExtra"))
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
      selectInput(
        inputId = ns("selectHow"),
        label = "Across-time",
        choices = myvals$choices,
        selected = "equal_weight",
        multiple = TRUE
    )
  })
  
  output$selectHowUIExtra <- renderUI({
    fluidRow(
      column(width = 6,
             selectInput(
               inputId = ns("selectFrequency"),
               label = "Frequency",
               choices = c("day", "week", "month", "year"),
               selected = "month",
               multiple = FALSE)
             ),
      column(width = 6,
             numericInput(
               inputId = ns("selectLag"),
               label = "Window",
               min = 1,
               value = 12)
             )
      )
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

