
valence_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("valenceUI"))
  )
}

valence_server <- function(input, output, session) {
  ns <- session$ns

  myvals <- reactiveValues(
    valenceList = list_valence_shifters,
    choices = names(list_valence_shifters),
    selected = NULL,
    useValence = FALSE,
    method = "Bigram",
    methodChoices = c("Bigram", "Cluster")
  )

  valenceFileName <- reactive({
    if (!is.null(input$valenceUpload$datapath)) {
      return(sub(".csv$", "", basename(input$valenceUpload$name)))
    } else {
      return(NULL)
    }
  })

  observeEvent(input$valenceUpload, {

    selected <- input$selectValence

    df <- read.csv(input$valenceUpload$datapath,
                   header = TRUE,
                   sep = ";",
                   quote = '"',
                   fileEncoding = "UTF-8",
                   stringsAsFactors = FALSE)

    if (all(c("x", "y") %in% colnames(df)) && !"t" %in% colnames(df) ||
        all(c("x", "t") %in% colnames(df)) && !"y" %in% colnames(df)) {

      existingValenceNames <- myvals$choices
      newValenceFileName <- valenceFileName()

      if (newValenceFileName %in% existingValenceNames){
        showModal(modalDialog(
          title = "Warning",
          paste("A set of valence shifters with the name: '", newValenceFileName, "' already exists.")
        ))
      } else {
        x <- list(data.table::data.table(df))
        names(x) <- newValenceFileName
        myvals$valenceList <- c(myvals$valenceList, x)
        myvals$choices <- names(myvals$valenceList)
        selected <- newValenceFileName
        showModal(modalDialog(
          title = "Success",
          paste("Valence shifters with the name: '" , newValenceFileName,
                "' added to the list of valence shifters.")
        ))
      }
    } else {
      showModal(modalDialog(
        title = "Error",
        "Columns 'x' and 'y' or 't' not found. Please upload a valid file."
      ))
    }

    updateSelectizeInput(session = getDefaultReactiveDomain(),
                         inputId = "selectValence", selected = selected)

  })

  output$valenceUI <- renderUI({
      tags$table(
        id = "inputs-table",
        style = "width: 100%",
        tags$tr(
          tags$td(
            style = "width: 90%",
            radioGroupButtons(
              inputId = ns("valenceMethod"),
              label = "Valence shifting",
              choices = c("Unigram", "Bigram", "Cluster"),
              justified = TRUE
            )
          ),
          tags$td(
            style = "width: 10%",
            div(class = "form-group shiny-input-container",
                style = "margin-top: 25px",
                actionButton(
                  inputId = ns("valenceMethodHelpButton"),
                  label = NULL,
                  icon = icon("question")
                )
            )
          )
        ),
        tags$tr(
          tags$td(
            style = "width: 90%",
            selectizeInput(
              inputId = ns("selectValence"),
              label = "Select valence shifters from list or upload",
              choices = c("none", myvals$choices),
              selected = NULL,
              multiple = FALSE
            )
          ),
          tags$td(
            style = "width: 10%",
            uiOutput(ns("loadValenceUI"))
          )
        )
      )
  })

  output$loadValenceUI <- renderUI({
    dropdownButton(
      tags$h4("Upload valence shifters"),
      tags$table(
        id = "inputs-table",
        style = "width: 80%",
        tags$tr(
          tags$td(
            style = "width: 80%",
            fileInput(
              inputId = ns("valenceUpload"),
              label = "Choose .csv file",
              multiple = FALSE,
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            )
          ),
          tags$td(
            style = "width: 10%",
            div(class = "form-group shiny-input-container",
                actionButton(
                  inputId = ns("valenceHelpButton"),
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

  observeEvent(input$valenceHelpButton, {
    showModal(modalDialog(
      title = "Upload valence shiters",
      "The .csv file should contain two headers named 'x' and 'y', or 'x' and 't'. Only one set of valence shifters
      can be uploaded at the same time. Once you have uploaded the file, the set of valence shifters will be
      available in the predefined list. The name of the set of valence shifters will be the filename of the uploaded
      set of valence shifters. Use ';' for the separation of columns in the file."
    ))
  })

  observeEvent(input$selectValence, {

    if (input$selectValence != "none") {
      myvals$selected <- input$selectValence
      myvals$useValence <- TRUE
      colnames <- names(myvals$valenceList[[input$selectValence]])
      if (all(c("y", "t") %in% colnames)) {
        myvals$methodChoices <- c("Bigram", "Cluster")
        myvals$method <- "Bigram"
      } else if ("y" %in% colnames) {
        myvals$methodChoices <- c("Bigram")
        myvals$method <- "Bigram"
      } else {
        myvals$methodChoices <- c("Cluster")
        myvals$method <- "Cluster"
      }
      updateRadioGroupButtons(session, "valenceMethod",
                              choices = myvals$methodChoices,
                              selected = myvals$method)
    } else {
      myvals$selected <- NULL
      myvals$useValence <- FALSE
    }

  })

  observeEvent(input$valenceMethodHelpButton, {
    showModal(modalDialog(
      title = "Valence shifting method",
      "If both the columns 'y' and 't' are delivered, you need to choose between the bigram
      or the cluster approach. For the bigram approach, column 'y' is used. For the cluster,
      approach column 't' is used."
    ))
  })

  return(myvals)
}

