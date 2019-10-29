
load_corpus_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("corpusUI"))
  )
}

load_corpus_server <- function(input, output, session) {
  ns <- session$ns
  
  output$corpusUI <- renderUI({
    tags$table(
      id = "inputs-table",
      style = "width: 50%",
      tags$h4("Corpus"),
      tags$tr(
        tags$td(
          style = "width: 90%",
          fileInput(
            inputId = ns("corpusUpload"),
            label = "Upload .csv file",
            multiple = FALSE,
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
            placeholder = "corpus.csv"
          )
        ),
        tags$td(
          style = "width: 5%",
          div(class = "form-group shiny-input-container",
              actionButton(
                inputId = ns("corpusHelpButton"),
                label = NULL,
                icon = icon("question")
              )
          )
        )
      )
    )
  })
  
  dt <- as.data.table(read.csv(system.file("extdata", "corpus.csv", package = "sentometrics.app"),
                               header = TRUE,
                               sep = ";",
                               quote = '"',
                               fileEncoding = "UTF-8",
                               stringsAsFactors = FALSE)
  )
  dt[, id := as.character(id)]
  corpusFile <- reactiveVal(dt)

  observeEvent(input$corpusUpload, ignoreNULL = TRUE, ignoreInit = TRUE, {

    dt <- as.data.table(
      read.csv(input$corpusUpload$datapath,
               header = TRUE,
               sep = ";",
               quote = '"',
               fileEncoding = "UTF-8",
               stringsAsFactors = FALSE)
    )

    w <- match(c("id", "date", "texts"), names(dt), nomatch = 0)
    setcolorder(dt, colnames(dt)[w])

    if ("texts" %in% colnames(dt)) {
      corpusFile(dt)
    } else {
      showModal(modalDialog(
        title = "Error",
        "No column 'texts' found. Please upload a valid file."
      ))
    }
  })

  observeEvent(input$corpusHelpButton, {
    showModal(modalDialog(
      title = "Upload a corpus",
      "The .csv file should at a very minimun contain a header named 'texts'. However, it is recommended
      to also have an 'id' and a properly formatted 'date' column so a sento_corpus can be created. The
      file can also contain additional columns for (numeric) features. Use ';' for the separation of
      columns in the file."
    ))
  })

  return(corpusFile)
}

render_corpus_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(DT::dataTableOutput(ns("corpusTable")), style = "font-size:80%")
  )
}

render_corpus_server <- function(input, output, session, corpusFile) {
  ns <- session$ns
  
  colNumTexts <- reactive({
    grep("texts", colnames(corpusFile()))
  })

  output$corpusTable <- DT::renderDataTable({

    corp <- corpusFile()
    cols <- colnames(corp[, sapply(corp, is.numeric), with = FALSE])

    DT::datatable(corp, options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 15, 20),
      columnDefs = list(list(
        targets = colNumTexts(),
        render = JS(
          "function(data, type, row, meta) {",
          "return data.length > 6 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
          "}")
      ))),
      callback = JS("table.page(3).draw(false);"
      )
    ) %>% formatRound(columns = cols, digits = 2)
  }, server = TRUE)
  
}

create_corpus_server <- function(input, output, session, corpusFile) {
  corpusData <- reactive({
    dt <- corpusFile()
    if (all(c("texts", "id", "date") %in% colnames(dt))) {
      dt$id <- as.character(dt$id)
      dt$date <- as.character(dt$date)
      dt$texts <- as.character(dt$texts)
      corp <- sentometrics::sento_corpus(dt)
    } else {
      corp <- as.character(dt$texts)
    }
  })
}

