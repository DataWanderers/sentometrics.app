
indices_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("selectIndex")),
    plotOutput(ns("indexPlot")) %>% withSpinner(color = "#0dc5c1"),
    downloadButton(ns("downloadIndex"), "Download indices")
  )
}

indices_server <- function(input, output, session, sento_measures) {
  ns <- session$ns

  vals <- reactiveValues(
    sento_measures = NULL,
    selectedGrouping = NULL
  )

  observe({
    vals$sento_measures <- sento_measures()
  })

  output$selectIndex <- renderUI({

    if (is.null(sento_measures())) {
      tags$p("Calculate sentiment first...")
    } else {
      selectizeInput(
        inputId = ns("select_index"),
        label = "Select a grouping to display",
        choices = c("all", "features", "lexicons", "time"),
        selected = "all",
        multiple = FALSE
      )
    }
  })

  observe({
    vals$selectedGrouping <- input$select_index
  })

  output$indexPlot <- renderPlot({
    plot(vals$sento_measures, group = vals$selectedGrouping)
  })

  output$downloadIndex <- downloadHandler(
    filename = function() paste("sentiment_indices_", Sys.Date(), ".csv", sep = ""),
    content = function(con) {
      write.csv(as.data.table(vals$sento_measures), con)
    },
    contentType = "text/csv"
  )

}

