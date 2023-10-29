
library("shiny")
library("shinyjs")
library("shinyWidgets")
library("shinythemes")
library("shinycssloaders")
library("DT")
library("tableHTML")
library("sentometrics")
library("quanteda")
library("data.table")

source("header.R")
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
options(scipen = 999, shiny.maxRequestSize = 50*1024^2)


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  shinyjs::useShinyjs(),
  extendShinyjs(script = "extensions.js", functions = c("init")),
  tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "stylesheet", type = "text/css",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
  ),
  tags$style(type = "text/css",
             ".form-control.shiny-bound-input, .selectize-input {height: 35px;}"),
  
  tagList(
    div(id = "loading-content", h2("Loading..."))
  ),
  
  headerPanel(tags$a(href = "https://github.com/DataWanderers/sentometrics.app",
                     target = "_blank",
                     class = "fa fa-github",
                     style = "font-size:24px",
                     id = "github",
                     " sentometrics.app"),
              windowTitle = "sentometrics.app"),
  
  sidebarPanel(
      tags$style(tableHTML::make_css(list(".well", "border-width", "5px"))),
      style = "margin: 5px; padding-top: 10px; padding-bottom: 0px",
      
      header_ui("header_ui"),
      
      tags$tr(tags$h4("Word lists")),
      lexicon_ui("lexicon_ui"),
      valence_ui("valence_ui"),
      
      tags$tr(tags$h4("Aggregation")),
      fluidRow(
        column(width = 6, howWithin_ui("howWithin_ui")),
        column(width = 6, howDocs_ui("howDocs_ui"))
      ),
      howTime_ui("howTime_ui")
  ),
  mainPanel(
    id = "main_panel",
    fluidRow(
        style = "margin: 0px",
        tabsetPanel(
            id = "tabs",
            tabPanel(
                style = "margin: 0px",
                title = "Corpus",
                load_corpus_ui("load_corpus_ui"),
                render_corpus_ui("corpusTable")
            ),
            tabPanel(
                style = "margin: 0px",
                title = "Summary",
                corpus_summary_ui("corpus_summary_ui")
            ),
            tabPanel(
                style = "margin: 0px",
                title = "Sentiment",
                sentiment_ui("sentiment_ui"),
                value = "sentimentTab"
            ),
            tabPanel(
                style = "margin: 0px",
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
  valenceMethod = "Unigrams",
  sento_measures = NULL,
  sentiment = NULL,
  by = NULL,
  lag = 1
)

server <- function(input, output, session) {
  observe({
    if (!is.null(myvals$sentiment)) {
      shinyjs::show(select = "#main_panel li a[data-value='sentimentTab']")
    }
  })

  observe({
    if (!is.null(myvals$sento_measures)) {
      shinyjs::show(selec = "#main_panel li a[data-value='indicesTab']")
    } else {
      shinyjs::hide(selec = "#main_panel li a[data-value='indicesTab']")
    }
  })

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

  corpusFile <- callModule(load_corpus_server, "load_corpus_ui")
  corpus <- callModule(create_corpus_server, "", corpusFile)
  callModule(render_corpus_server, "corpusTable", corpusFile)
  
  corpusSummaryModule <- callModule(corpus_summary_server, "corpus_summary_ui", corpus)

  sentoLexicon <- callModule(build_sento_lexicon, "", myvals)

  observeEvent(input$calcSentimentButton, ignoreInit = FALSE, {
    if (is.null(sentoLexicon())) {
      showModal(modalDialog(
        title = "Error",
        "Provide a corpus and/or lexicon(s) first..."
      ))
    } else {
      updateTabsetPanel(session, "tabs", selected = "sentimentTab")
      sentimentModule <- callModule(sentiment_server, "sentiment_ui", myvals,
                                    corpus, sentoLexicon, input$calcSentimentButton)
      observe({
        myvals$sento_measures <- sentimentModule$sento_measures
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
  
  hide(id = "loading-content", anim = TRUE, animType = "fade") 
}

shinyApp(ui = ui, server = server)

