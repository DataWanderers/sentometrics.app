
library("shiny")
library("shinyWidgets")
library("shinythemes")
library("shinycssloaders")
library("DT")
library("tableHTML")

library("sentometrics")
library("quanteda")
library("data.table")

source("corpus.R")
source("lexicon.R")
source("valence.R")
source("howWithin.R")
source("howDocs.R")
source("howTime.R")
source("corpusSummary.R")
source("sentiment.R")
source("indices.R")

data("list_lexicons", package = "sentometrics")
data("list_valence_shifters", package = "sentometrics")
options(scipen = 999)
options(shiny.maxRequestSize = 50*1024^2)

ui <- fluidPage(
    theme = shinytheme("cerulean"),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "css.css")
    ),
    sidebarPanel(
        tags$style(tableHTML::make_css(list('.well', 'border-width', '10px'))),
        style = "margin: 14px",
        header_ui("header_ui"),
        lexicon_ui("lexicon_ui"),
        valence_ui("valence_ui"),
        howWithin_ui("howWithin_ui"),
        howDocs_ui("howDocs_ui"),
        howTime_ui("howTime_ui")
    ),
    mainPanel(
        fluidRow(
            style = "margin: 15px",
            tabsetPanel(
                id = "tabs",
                tabPanel(
                    style = "margin: 15px",
                    title = "Corpus",
                    load_corpus_ui("load_corpus_ui"),
                    render_corpus_ui("corpusTable")
                ),
                tabPanel(
                    style = "margin: 15px",
                    title = "Summary",
                    corpus_summary_ui("corpus_summary_ui")
                ),
                tabPanel(
                    style = "margin: 15px",
                    title = "Sentiment",
                    sentiment_ui("sentiment_ui"),
                    value = "sentimentTab"
                ),
                tabPanel(
                    style = "margin: 15px",
                    title = "Indices",
                    indices_ui("indices_ui"),
                    value = "indicesTab"
                )
            )
        )
    )
)

myvals <- reactiveValues(
    selectedLexicons = NULL,
    selectedValence = NULL,
    useValence = FALSE,
    lexiconList = list_lexicons,
    valenceList = list_valence_shifters,
    howWithin = NULL,
    howDocs = NULL,
    howTime = NULL,
    valenceMethod = "Bigrams",
    sento_measures = NULL,
    sentiment = NULL,
    by = NULL,
    lag = 1
)

server <- function(input, output, session) {

    observe({
        if (is.null(myvals$sento_measures)) {
            hideTab(inputId = "tabs", target = "indicesTab")
        } else {
            showTab(inputId = "tabs", target = "indicesTab")
        }
    })

    observe({
        if (is.null(myvals$sentiment)) {
            hideTab(inputId = "tabs", target = "sentimentTab")
        } else {
            showTab(inputId = "tabs", target = "sentimentTab")
        }
    })

    corpusFile <- callModule(load_corpus_server, "load_corpus_ui")
    corpus <- callModule(create_corpus_server, "", corpusFile)
    callModule(render_corpus_server, "corpusTable", corpusFile)

    lexModule <- callModule(lexicon_server, "lexicon_ui")
    observe({
        myvals$selectedLexicons <- lexModule$selected
        myvals$lexiconList <- lexModule$lexiconList
    })

    valenceModule <- callModule(valence_server, "valence_ui")
    observe({
        myvals$selectedValence <- valenceModule$selected
        myvals$useValence <- valenceModule$useValence
        myvals$valenceMethod <- valenceModule$method
        myvals$valenceList <- valenceModule$valenceList
    })
    
    howWithinModule <- callModule(howWithin_server, "howWithin_ui")
    observe({
        myvals$howWithin <- howWithinModule$selected
    })

    howDocsModule <- callModule(howDocs_server, "howDocs_ui")
    observe({
        myvals$howDocs <- howDocsModule$selected
    })

    howTimeModule <- callModule(howTime_server, "howTime_ui")
    observe({
        myvals$howTime <- howTimeModule$selected
        myvals$by <- howTimeModule$by
        myvals$lag <- howTimeModule$lag
    })

    corpusSummaryModule <- callModule(corpus_summary_server, "corpus_summary_ui", corpus)

    sentoLexicon <- callModule(build_sento_lexicon, "", myvals)

    observeEvent(input$calcSentimentButton, ignoreInit = FALSE, {

            if (is.null(sentoLexicon())) {
                showModal(modalDialog(
                    title = "Error",
                    "Select a corpus and/or lexicon(s) first..."
                ))
            } else {
                showTab(inputId = "tabs", target = "sentimentTab")
                updateTabsetPanel(session, "tabs", selected = "sentimentTab")
                sentimentModule <- callModule(sentiment_server, "sentiment_ui", myvals,
                                              corpus, sentoLexicon, input$calcSentimentButton)
                observe({
                    if (!is.null(sentimentModule$sento_measures)) {
                        myvals$sento_measures <- sentimentModule$sento_measures
                    } else {
                        myvals$sento_measures <- NULL
                    }
                })
                observe({
                    if (!is.null(sentimentModule$sentiment)) {
                        myvals$sentiment <- data.table::as.data.table(sentimentModule$sentiment)
                    } else {
                        myvals$sentiment <- NULL
                    }
                })
            }
    })
    callModule(indices_server, "indices_ui", reactive(myvals$sento_measures))
}

shinyApp(ui = ui, server = server)

