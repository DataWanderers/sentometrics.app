
lexicon_ui <- function(id) {
    ns <- NS(id)
    tags$table(id = "inputs-table", style = "width: 100%",
        tags$tr(tags$td(style = "width: 90%", uiOutput(ns("selectLexiconsUI"))),
            tags$td(style = "padding-left: 10px; width: 10%",
                uiOutput(ns("loadLexiconUI")))))
}

lexicon_server <- function(input, output, session) {

    myvals <- reactiveValues(lexiconList = list_lexicons,
        choices = names(list_lexicons), selected = NULL)

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
            header = TRUE, sep = ";", quote = "\"",
            fileEncoding = "UTF-8", stringsAsFactors = FALSE)

        if (all(c("x", "y") %in% colnames(df))) {
            existingLexiconsNames <- myvals$choices
            newLexiconFileName <- lexiconFileName()

            if (newLexiconFileName %in% existingLexiconsNames) {
                showModal(modalDialog(title = "Warning",
                  paste("A lexicon with the name: '",
                    newLexiconFileName, "' already exists.")))
            } else {
                x <- list(data.table::data.table(df))
                names(x) <- newLexiconFileName
                myvals$lexiconList <- c(myvals$lexiconList,
                  x)
                myvals$choices <- names(myvals$lexiconList)
                selected <- c(selected, newLexiconFileName)
                showModal(modalDialog(title = "Success",
                  paste0("Lexicon with the name: '",
                    newLexiconFileName, "' added to the list of lexicons.")))
            }
        } else {
            showModal(modalDialog(title = "Error",
                "Columns 'x' and/or 'y' not found. Please upload a valid file."))
        }

        updateSelectizeInput(session = getDefaultReactiveDomain(),
            inputId = "selectLexicons", selected = selected)

    })

    output$selectLexiconsUI <- renderUI({
        selectizeInput(inputId = session$ns("selectLexicons"),
            label = "Select or upload lexicons", choices = as.list(myvals$choices),
            selected = "GI_en", multiple = TRUE)
    })

    output$loadLexiconUI <- renderUI({
        dropdownButton(tags$h4("Upload lexicon"), tags$table(id = "inputs-table",
            style = "width: 80%", tags$tr(tags$td(style = "width: 80%",
                fileInput(inputId = session$ns("lexiconUpload"),
                  label = "Choose .csv file", multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain",
                    ".csv"))), tags$td(style = "width: 10%",
                div(class = "form-group shiny-input-container",
                  actionButton(inputId = session$ns("lexiconHelpButton"),
                    label = NULL, icon = icon("question")))))),
            circle = TRUE, status = "danger", icon = icon("upload"),
            width = "300px", tooltip = tooltipOptions(title = "Click to upload!"))
    })

    observeEvent(input$lexiconHelpButton, {
        showModal(modalDialog(title = "Upload lexicon",
            "The .csv file (with ';' as separator) should contain the headers 'x'
      and 'y'. Only one lexicon can be uploaded at a time. Once uploaded, the
      lexicon becomes available in the list using the filename as identifier."))
    })

    observe({
        myvals$selected <- input$selectLexicons
    })

    return(myvals)
}

build_sento_lexicon <- function(input, output, session,
    params) {
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
            valenceShiftersIn <- data.table::as.data.table(valenceList[[selectedValence]])
            if (valenceMethod == "Bigrams") {
                valenceShiftersIn <- valenceShiftersIn[,
                  .(x, y)]
            } else if (valenceMethod == "Clusters") {
                valenceShiftersIn <- valenceShiftersIn[,
                  .(x, t)]
            }
        } else {
            valenceShiftersIn <- NULL
        }
        if (!is.null(lexiconsIn)) {
            lex <- sento_lexicons(lexiconsIn = lexiconsIn,
                valenceIn = valenceShiftersIn)
        } else {
            lex <- NULL
        }

        lex
    })
}

