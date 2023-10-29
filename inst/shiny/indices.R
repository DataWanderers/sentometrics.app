
indices_ui <- function(id) {
    ns <- NS(id)
    tagList(tags$h4("Investigate aggregated time series indices"),
        uiOutput(ns("selectIndex")), plotOutput(ns("indexPlot")) %>%
            withSpinner(color = "#0dc5c1"), downloadButton(ns("downloadIndex"),
            "Download indices"))
}

indices_server <- function(input, output, session,
    sento_measures) {
    ns <- session$ns

    vals <- reactiveValues(sento_measures = NULL, selectedGrouping = "all")

    observe({
        vals$sento_measures <- sento_measures()
    })

    output$selectIndex <- renderUI({
        selectizeInput(inputId = ns("select_index"),
            label = "Grouping", choices = c("all",
                "features", "lexicons", "time"), selected = "all",
            multiple = FALSE)
    })

    observeEvent(input$select_index, {
        vals$selectedGrouping <- input$select_index
    })

    output$indexPlot <- renderPlot({
        plot(vals$sento_measures, group = vals$selectedGrouping) +
            ggplot2::theme(text = ggplot2::element_text(size = 16))
    })

    output$downloadIndex <- downloadHandler(filename = function() paste("sentiment_indices_",
        Sys.Date(), ".csv", sep = ""), content = function(file) {
        write.csv(as.data.table(vals$sento_measures),
            file, row.names = FALSE)
    }, contentType = "text/csv")
}

