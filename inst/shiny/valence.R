
valence_ui <- function(id) {
  ns <- NS(id)
  tags$table(
    id = "inputs-table",
    style = "width: 100%",
    tags$tr(
      tags$td(
        style = "width: 90%",
        uiOutput(ns("selectValenceUI"))
      ),
      tags$td(
        style = "padding-left: 10px; width: 10%",
        uiOutput(ns("loadValenceUI"))
      )
    ),
    tags$tr(
      tags$td(
        style = "width: 90%",
        uiOutput(ns("methodValenceUI"))
      ),
      tags$td(
        style = "width: 10%",
        uiOutput(ns("methodValenceHelpUI"))
      )
    )
  )
}

valence_server <- function(input, output, session) {
  ns <- session$ns

  myvals <- reactiveValues(
    valenceList = list_valence_shifters,
    choices = names(list_valence_shifters),
    selected = NULL,
    useValence = FALSE,
    method = "Bigrams",
    choicesMethod = c("Unigrams", "Bigrams", "Clusters")
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

    if ("x" %in% colnames(df) &&
        (("y" %in% colnames(df)) || ("z" %in% colnames(df)))) {
      existingValenceNames <- myvals$choices
      newValenceFileName <- valenceFileName()
      if (newValenceFileName %in% existingValenceNames) {
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
        "Columns 'x' and 'y' and/or 't' not found. Please upload a valid file."
      ))
    }

    updateSelectizeInput(session = getDefaultReactiveDomain(),
                         inputId = "selectValence", selected = selected)
  })
  
  output$selectValenceUI <- renderUI({
    selectizeInput(
      inputId = ns("selectValence"),
      label = "Select or upload valence shifters",
      choices = c("none", myvals$choices),
      selected = "none",
      multiple = FALSE
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

  output$methodValenceUI <- renderUI({
    radioGroupButtons(
      inputId = ns("valenceMethod"),
      label = "Valence shifting",
      choices = c("Unigrams"),
      justified = TRUE
    )
  })
  
  output$methodValenceHelpUI <- renderUI({
    div(class = "form-group shiny-input-container",
        style = "margin-top: 25px",
        actionButton(
          inputId = ns("valenceMethodHelpButton"),
          label = NULL,
          icon = icon("question")
        )
    )
  })
  
  observeEvent(input$valenceHelpButton, {
    showModal(modalDialog(
      title = "Upload valence shifters",
      "The .csv file (with ';' as separator) should contain the headers 'x' and 'y'
      and/or 't'. Only one set of valence shifters can be uploaded at a time. Once
      uploaded, the set of valence shifters becomes available in the list using
      the filename as identifier."
    ))
  })

  observeEvent(input$selectValence, {
    if (input$selectValence != "none") {
      myvals$selected <- input$selectValence
      myvals$useValence <- TRUE
      myvals$choicesMethod <- c("Bigrams", "Clusters")  # fallback
      
      cols <- names(myvals$valenceList[[input$selectValence]])
      if (all(c("y", "t") %in% cols)) {
        if (input$valenceMethod == "Unigrams") {
          myvals$method <- "Bigrams"  # default first pick
        } else {
          myvals$method <- input$valenceMethod
        }
      } else {
        if ("y" %in% cols) {
          myvals$choicesMethod <- myvals$method <- "Bigrams"
        } else if ("t" %in% cols) {
          myvals$choicesMethod <- myvals$method <- "Clusters"
        }
      }
    } else if (input$selectValence == "none") {
      myvals$selected <- NULL
      myvals$useValence <- FALSE
      myvals$choicesMethod <- myvals$method <- "Unigrams"
    }
    
    updateRadioGroupButtons(session, "valenceMethod", 
                            choices = myvals$choicesMethod, selected = myvals$method)
  }, ignoreInit = TRUE)

  observeEvent(input$valenceMethod, {
    myvals$method <- input$valenceMethod
  })

  observeEvent(input$valenceMethodHelpButton, {
    showModal(modalDialog(
      title = "Valence shifting method",
      "If you give both columns 'y' and 't', you need to choose between the bigrams approach
      (column 'y') or the clusters approach (column 't'). The unigrams approach applies when
      no valence shifters are provided."
    ))
  })

  return(myvals)
}

