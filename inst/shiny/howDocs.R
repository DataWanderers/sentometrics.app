
howDocs_ui <- function(id) {
    ns <- NS(id)
    tags$table(id = "inputs-table", style = "width: 100%",
        tags$tr(tags$td(uiOutput(ns("selectHowUI")))))
}

howDocs_server <- function(input, output, session) {
    ns <- session$ns

    myvals <- reactiveValues(selected = NULL, choices = sentometrics::get_hows()$docs)

    output$selectHowUI <- renderUI({
        selectInput(inputId = ns("selectHow"), label = "Across-document",
            choices = myvals$choices, selected = myvals$selected,
            multiple = FALSE)
    })

    observeEvent(input$selectHow, {
        myvals$selected <- input$selectHow
    })

    return(myvals)
}

