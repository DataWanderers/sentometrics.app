
header_ui <- function(id) {
  ns <- NS(id)
  tags$table(
    id = "inputs-table",
    style = "width: 100%",
    tags$tr(
      tags$td(width = "70%", tags$h3(style = "margin: 0px", "Parameters")),
      tags$td(actionButton(
        inputId = "calcSentimentButton",
        label = "Calculate!",
        icon = icon("rocket"))
      )
    )
  )
}

lexicon_ui <- function(id) {
  ns <- NS(id)
  tags$table(
    id = "inputs-table",
    style = "width: 100%",
    tags$tr(tags$h4("Word lists")),
    tags$tr(
      tags$td(
        style = "width: 85%",
        uiOutput(ns("selectLexiconsUI"))
      ),
      tags$td(
        style = "width: 10%",
        uiOutput(ns("loadLexiconUI"))
      )
    )
  )
}

lexicon_server <- function(input, output, session) {

  myvals <- reactiveValues(
    lexiconList = list_lexicons,
    choices = names(list_lexicons),
    selected = NULL
  )

  lexiconFileName <- reactive({
    if (!is.null(input$lexiconUpload$datapath)) {
      return(sub(".csv$", "", basename(input$lexiconUpload$name)))
    } else {
      return(NULL)
    }
  })

  observeEvent(input$lexiconUpload, {

    selected <- input$selectLexicons

    df <- read.csv(input$lexiconUpload$datapath,
                   header = TRUE,
                   sep = ";",
                   quote = '"',
                   fileEncoding = "UTF-8",
                   stringsAsFactors = FALSE)

    if (all(c("x", "y") %in% colnames(df))) {
      existingLexiconsNames <-  myvals$choices
      newLexiconFileName <- lexiconFileName()

      if (newLexiconFileName %in% existingLexiconsNames) {
        showModal(modalDialog(
          title = "Warning",
          paste("A lexicon with the name: '", newLexiconFileName, "' already exists.")
        ))
      } else {
        x <- list(data.table::data.table(df))
        names(x) <- newLexiconFileName
        myvals$lexiconList <- c(myvals$lexiconList, x)
        myvals$choices <- names(myvals$lexiconList)
        selected <- c(selected, newLexiconFileName)
        showModal(modalDialog(
          title = "Success",
          paste0("Lexicon with the name: '" , newLexiconFileName, "' added to the list of lexicons.")
        ))
      }
    } else {
      showModal(modalDialog(
        title = "Error",
        "Columns 'x' and/or 'y' not found. Please upload a valid file."
      ))
    }
    
    updateSelectizeInput(session = getDefaultReactiveDomain(),
                         inputId = "selectLexicons", selected = selected
    )

  })

  output$selectLexiconsUI <- renderUI({
    selectizeInput(
      inputId = session$ns("selectLexicons"),
      label = "Select lexicons from list or upload",
      choices = as.list(myvals$choices),
      selected = "GI_en",
      multiple = TRUE
    )
  })

  output$loadLexiconUI <- renderUI({
    dropdownButton(
      tags$h4("Upload lexicon"),
      tags$table(
        id = "inputs-table",
        style = "width: 80%",
        tags$tr(
          tags$td(
            style = "width: 80%",
            fileInput(
              inputId = session$ns("lexiconUpload"),
              label = "Choose .csv file",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            )
          ),
          tags$td(
            style = "width: 10%",
            div(class = "form-group shiny-input-container",
                actionButton(
                  inputId = session$ns("lexiconHelpButton"),
                  label = NULL,
                  icon = icon("question")
                )
            )
          )
        )
      ),
      circle = TRUE, status = "danger",
      icon = icon("upload"), width = "300px",
      tooltip = tooltipOptions(title = "Click to upload!")
    )
  })

  observeEvent(input$lexiconHelpButton, {
    showModal(modalDialog(
      title = "Upload a lexicon",
      "The .csv file should contain two headers named 'x' and 'y'. Only one lexicon can be uploaded at the same time.
      Once you have uploaded the file, the lexicon will be available in the predefined list. The name of your
      lexicon will be the filename of the uploaded lexicon. Use ';' for the separation of columns in the file."
    ))
  })

  observe({
    myvals$selected <- input$selectLexicons
  })

  return(myvals)
}

build_sento_lexicon <- function(input, output, session, params) {
  
  sentoLexicon <- reactive({
    lexiconList <- params$lexiconList
    selectedLexicons <- params$selectedLexicons
    useValence <- params$useValence
    selectedValence <- params$selectedValence
    valenceMethod <- params$valenceMethod
    valenceList <- params$valenceList
    
    if (!is.null(selectedLexicons)) {
      lexiconsIn <- c(lexiconList[selectedLexicons])
    } else {
      return(NULL)
    }
    if (useValence && !is.null(selectedValence)) {
      valenceShiftersIn <- as.data.table(valenceList[[selectedValence]])
      if (valenceMethod == "Bigram") {
        valenceShiftersIn <- valenceShiftersIn[, .(x, y)]
      } else {
        valenceShiftersIn <- valenceShiftersIn[, .(x, t)]
      }
    } else {
      valenceShiftersIn <- NULL
    }
    if (!is.null(lexiconsIn)) {
      lex <- sento_lexicons(lexiconsIn = lexiconsIn, valenceIn = valenceShiftersIn)
    } else {
      lex <- NULL
    }
    lex
  })
  
}
