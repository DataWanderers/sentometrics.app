
corpus_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h4("Get an overview of the corpus"),
    uiOutput(ns("corpusSummary"))
  )
}

corpus_summary_server <- function(input, output, session, corpus) {
  ns <- session$ns

  myvals <- reactiveValues(
    frequency = "day"
  )

  corpusSummary <- reactive({
    req(corpus())
    sentometrics::corpus_summarize(corpus(), by = myvals$frequency)
  })

  output$summaryStatsTable <- DT::renderDataTable({
    tokeep <- which(sapply(corpusSummary()$stats, is.numeric))
    cols <- colnames(corpusSummary()$stats[, tokeep, with = FALSE])
    DT::datatable(corpusSummary()$stats, options = list(searching = FALSE)) %>%
      formatRound(columns = cols, digits = 0)
  }, server = TRUE)

  output$downloadCorpusSummary <- downloadHandler(
    filename = function() paste("corpus_summary_stats_", Sys.Date(), ".csv", sep = ""),
    content = function(con) {
      write.csv(corpusSummary()$stats, con)
    },
    contentType = "text/csv"
  )

  output$docPlot <- renderPlot({
    corpusSummary()$plots$doc_plot + ggplot2::theme(text = ggplot2::element_text(size=16))
  })

  output$tokenPlot <- renderPlot({
    corpusSummary()$plots$token_plot + ggplot2::theme(text = ggplot2::element_text(size=16))
  })

  output$featurePlot <- renderPlot({
    corpusSummary()$plots$feature_plot + ggplot2::theme(text = ggplot2::element_text(size=16))
  })
  
  observeEvent(input$selectSummaryFrequency, {
    myvals$frequency <- input$selectSummaryFrequency
  })

  output$corpusSummary <- renderUI({
    validate(
      need("sento_corpus" %in% class(corpus()),
           "Your corpus is missing the columns 'id' and 'date' meaning no time series can be computed.")
    )

    fluidRow(
      style = "margin: 0px",
      tags$div(
        style = "margin-bottom: 15px",
        selectizeInput(
          inputId = ns("selectSummaryFrequency"),
          label = "Frequency",
          choices = c("day", "week", "month", "year"),
          selected = "day",
          multiple = FALSE
        )
      ),
      tabsetPanel(
        tabPanel(
          style = "margin: 15px",
          title = "Documents",
          plotOutput(ns("docPlot")) %>% withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Tokens",
          plotOutput(ns("tokenPlot")) %>% withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Features",
          plotOutput(ns("featurePlot")) %>% withSpinner(color = "#0dc5c1")
        ),
        tabPanel(
          style = "margin: 15px",
          title = "Statistics",
          div(dataTableOutput(ns("summaryStatsTable")) %>%
            withSpinner(color = "#0dc5c1"), style = "font-size:80%"),
          downloadButton(ns("downloadCorpusSummary"), "Download corpus statistics")
        )
      )
    )
  })
}

