
sentiment_ui <- function(id) {
    ns <- NS(id)
    tagList(tags$h4("Inspect document-level sentiment values"),
        uiOutput(ns("sentimentUI")))
}

sentiment_server <- function(input, output, session,
    params, corpus, sentoLexicon, calculate) {
    ns <- session$ns

    vals <- reactiveValues(selectedColumn = NULL, sento_measures = NULL,
        sentiment = NULL)

    output$downloadData <- downloadHandler(filename = function() paste("sentiment_",
        Sys.Date(), ".csv", sep = ""), content = function(file) {
        write.csv(vals$sentiment, file, row.names = FALSE)
    })

    output$downloadButtonConditional <- renderUI({
        downloadButton(ns("downloadData"), "Download sentiment table")
    })

    output$sentimentUI <- renderUI({
        output$sentimentTable <- DT::renderDataTable({
            tokeep <- which(sapply(vals$sentiment,
                is.numeric))
            cols <- colnames(vals$sentiment[, tokeep,
                with = FALSE])
            DT::datatable(vals$sentiment, options = list(searching = FALSE)) %>%
                formatRound(columns = cols, digits = 2)
        }, server = TRUE)

        output$selectSentiment <- renderUI({
            selectizeInput(inputId = ns("select_sentiment"),
                label = "Select a sentiment object to display",
                choices = as.list(colnamesSentiment()),
                selected = vals$selectedColumn, multiple = FALSE)
        })

        observe({
            vals$selectedColumn <- input$select_sentiment
        })

        fluidRow(style = "margin: 0px", div(dataTableOutput(ns("sentimentTable")),
            style = "font-size:80%"), uiOutput(ns("downloadButtonConditional")))
    })

    observeEvent(calculate, {
        if ("sento_corpus" %in% class(corpus())) {
            ctr <- sentometrics::ctr_agg(howWithin = params$howWithin,
                howDocs = params$howDocs, howTime = params$howTime,
                by = params$by, lag = params$lag, alphasExp = c(0.1,
                  0.4), aBeta = 1:2, bBeta = 1:2)
            sento_measures <- sentometrics::sento_measures(corpus(),
                sentoLexicon(), ctr)
            vals$sento_measures <- sento_measures
            vals$sentiment <- sento_measures$sentiment
        } else {
            sentiment <- compute_sentiment(corpus(),
                sentoLexicon(), how = params$howWithin)
            vals$sento_measures <- NULL
            vals$sentiment <- sentiment
        }
    })

    colnamesSentiment <- reactive({
        sent <- vals$sentiment
        if ("id" %in% colnames(sent)) {
            sent[, id := NULL]
        }
        if ("date" %in% colnames(sent)) {
            sent[, date := NULL]
        }
        col <- colnames(sent)
        names(col) <- col
        col
    })

    return(vals)
}

