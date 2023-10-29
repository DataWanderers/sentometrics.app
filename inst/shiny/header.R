
header_ui <- function(id) {
    ns <- NS(id)
    tags$table(id = "inputs-table", style = "padding: 0px; margin: 0px; width: 100%",
        tags$tr(tags$td(width = "100%", tags$h3(style = "margin: 0px",
            "Parameters")), tags$td(style = "padding-top: 5px",
            actionButton(inputId = "calcSentimentButton",
                label = "Calculate!", icon = icon("rocket")))))
}

