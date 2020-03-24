library(shiny)
library(shinyjs)
library(openxlsx)
library(magrittr)


shinyServer(function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE)
  
  observeEvent(input$file1, {
    shinyjs::enable("validate")
    ready$ok <- FALSE
  }) 
  
  observeEvent(input$validate, {
    shinyjs::disable("validate")
    ready$ok <- TRUE
  })  
  
  disableUI<-function(){
    shinyjs::disable("type")
    shinyjs::disable("ou")
    shinyjs::disable("ou_scheme")
    shinyjs::disable("de_scheme")
    shinyjs::disable("id_scheme")
    shinyjs::disable("ds_type")
    shinyjs::disable("file1")
    shinyjs::disable("header")
  }
  
  enableUI<-function(){
    shinyjs::enable("type")
    shinyjs::enable("ou")
    shinyjs::enable("ou_scheme")
    shinyjs::enable("de_scheme")
    shinyjs::enable("id_scheme")
    shinyjs::enable("ds_type")
    shinyjs::enable("file1")
    shinyjs::enable("header")
  }
  
  output$ui <- renderUI({
    
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("server"),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {
      fluidPage(
        tags$head(tags$style(".shiny-notification {
                             position: fixed;
                             top: 10%;
                             left: 33%;
                             right: 33%;}")),
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),
            selectInput("type", "Type:",
                        c(
                          "CSV" = "csv",
                          "JSON" = "json",
                          "XML" = "xml"
                        )),
            selectInput("ou", "Operating Unit", ous),
            selectInput(
              "de_scheme",
              "Data element ID scheme:",
              c(
                "ID" = "id",
                "Code" = "code",
                "Name" = "name"
              ),
              selected = "id"
            ),
            selectInput(
              "ou_scheme",
              "Orgunit ID scheme:",
              c(
                "ID" = "id",
                "Code" = "code",
                "Name" = "name"
              ),
              selected = "id"
            ),
            selectInput(
              "id_scheme",
              "ID scheme:",
              c(
                "ID" = "id",
                "Code" = "code",
                "Name" = "name"
              ),
              selected = "id"
            ),
            selectInput(
              "ds_type",
              "Dataset type:",
              c("Results" = "RESULTS", "Targets" = "TARGETS")
            ),
            checkboxInput("header", "CSV Header", TRUE),
            fileInput(
              "file1",
              "Choose data file:",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                "application/json",
                "application/xml",
                "application/zip",
                ".csv",
                ".json",
                ".xml",
                ".zip"
              )
            ),
            tags$hr(),
            actionButton("validate","Validate"),
            downloadButton("downloadData", "Download report")
          ),
          mainPanel(tabsetPanel(
            type = "tabs",
            tabPanel("Messages",   tags$ul(uiOutput('messages'))),
            tabPanel("Validation rules", dataTableOutput("contents"))
          ))
        ))
  }
})

  user_input <- reactiveValues(authenticated = FALSE, status = "")
  
  observeEvent(input$login_button, {
    is_logged_in<-FALSE
    user_input$authenticated <-DHISLogin(input$server,input$user_name,input$password)
    foo<-getUserOperatingUnits(getOption("organisationUnit"))
    ous<<-setNames(foo$id,foo$name)
  })   
  
  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    wellPanel(
      fluidRow(img(src='pepfar.png', align = "center")),
        fluidRow(h4("Welcome to the DATIM Validation tool. Please login with your DATIM credentials:")
      ),
      fluidRow(
      textInput("user_name", "User Name:",width = "600px"),
      passwordInput("password", "Password:",width = "600px"),
      actionButton("login_button", "Log in")
    ))
  })
  
  validate<-function() {
    shinyjs::hide("downloadData")
    if (!ready$ok) {return(NULL)}
    
    #Lock the UI and hide download button
    disableUI()
    inFile <- input$file1
    
    if (is.null(inFile)) return(NULL)
    
    messages<-list()
    vr_results<-list()
    has_error<-FALSE
    
    withProgress(message = 'Validating file', value = 0,{
      
    incProgress(0.1, detail = ("Loading metadata"))
    ds<-getCurrentMERDataSets(type = input$ds_type)
    incProgress(0.1, detail = ("Parsing data"))
    validation<-list()

    if  ( inFile$type == "application/zip" )  {
      temp_dir<-tempdir()
      input_file<-unzip(inFile$datapath, exdir = temp_dir)
    } else {
      input_file<-inFile$datapath
    }
    
    d <-  tryCatch({
      datimvalidation::d2Parser(
        filename = input_file,
        organisationUnit = input$ou,
        type = input$type,
        dataElementIdScheme = input$de_scheme,
        orgUnitIdScheme = input$ou_scheme,
        idScheme = input$id_scheme,
        csv_header = input$header
      )
    },
    error = function(e) {
      return(e)
    },
    warning = function(w) {
      list(paste("Escalated warning to error: ", conditionMessage(w)))
    })

      #Reset the button to force upload again
      shinyjs::reset("file1")
      disableUI()
      
      if (inherits(d, "list")) {
        messages <- append( "ERROR! : There were errors while parsing the file. Please check that you have provided the correct paramaters!", messages)
        messages <- append( d, messages)
        return(NULL)
        
      } else {
        
        messages<-append("No problems found during file parsing.",messages)
      }
      
      #Period check
      incProgress(0.1, detail = ("Checking periods."))
      
        messages <-  append(
          paste(
            paste( "Periods found: ", paste(unique(d$period),sep="",collapse=","))
          ),messages )
        
        #Record check
        incProgress(0.1, detail = ("Checking records: "))
        
        zero_check<-sprintf("%1.2f%%",  ( sum(as.character(d$value) == "0") / NROW(d) )  * 100 )
        
        messages <-  append(
          paste(
            paste( "Records found: ", NROW(d), " records found of which ", zero_check, " were zeros.")
          ),messages )
        
      #Duplicate check
      incProgress(0.1, detail = ("Checking for duplicate records."))
      
      dup_check <- getExactDuplicates(d)
      
      if (inherits(dup_check, "data.frame") & NROW(dup_check) > 0) {
        messages <-  append(
          paste(
            paste( NROW(dup_check)," duplicate values found.")
          ),messages )
        
        validation$duplicates_check<-dup_check
        
        has_error<-TRUE
        } else {
        messages<-append("No duplicate records detected.",messages)
      }
      
      incProgress(0.1, detail = ("Checking data element/orgunit associations"))
      de_ou_check <-
        checkDataElementOrgunitValidity(
          data = d,
          datasets = ds,
          organisationUnit = input$ou,
          return_violations = TRUE
        ) 
      
      if (inherits(de_ou_check, "data.frame")) {
        messages<-append(paste(
          NROW(de_ou_check),
          "invalid data element/orgunit associations found!"
        ), messages)
        
        validation$dataelement_ou_check<-de_ou_check
        
        has_error<-TRUE
      } else {
        messages<-append("Data element/orgunit associations are valid.", messages)
      }
      
      #Data element orgunit check
      incProgress(0.1, detail = ("Checking data element/disagg associations"))
      
      ds_disagg_check <-
        checkDataElementDisaggValidity(d, datasets = ds, return_violations = TRUE)
      
      if (inherits(ds_disagg_check, "data.frame")) {
        
        messages <- append(paste(
          NROW(ds_disagg_check),
          "invalid data element/disagg associations found!"
        ),
        messages)
        
        validation$datasets_disagg_check<-ds_disagg_check
        has_error<-TRUE
      } else {
        messages<-append("Data element/disagg associations are valid.",messages)
      }
      
      #Value type compliance check
      incProgress(0.1, detail = ("Checking value type compliance."))
      
      vt_check <- checkValueTypeCompliance(d)
      
      if (inherits(vt_check, "data.frame") & NROW(vt_check) > 0) {
        messages <-  append( paste( "ERROR! :", NROW(vt_check)," invalid values found."), messages)
        validation$value_type_compliance<-vt_check
        has_error<-TRUE
      } else {
        messages<-append("Value types are valid.",messages)
      }
      
      #Negative value check
      incProgress(0.1, detail = ("Checking negative numbers."))
      
      neg_check <- checkNegativeValues(d)
      
      if (inherits(neg_check, "data.frame")) {
        messages <- append(paste("ERROR! :", NROW(neg_check), " negatve values found."), messages)
        validation$negative_values <- neg_check
        has_error<-TRUE
      } else {
        messages<-append("No negative values found.",messages)
      }
      
      #Mechanism check
      incProgress(0.1, detail = ("Checking mechanisms."))
      
      mech_check <-
        checkMechanismValidity(
          data = d,
          organisationUnit = input$ou,
          return_violations = TRUE
        )
      
      if (inherits(mech_check, "data.frame")) {
        messages <- append(paste( "ERROR! :",
          NROW(mech_check), " invalid mechanisms found."
        ), messages)
        validation$mechanism_check <- mech_check
        has_error<-TRUE
      } else {
        messages <- append("All mechanisms are valid.", messages)
      }
      
      incProgress(0.1, detail = ("Validating data"))
      
      if ( Sys.info()['sysname'] == "Linux") {
        
       ncores <- parallel::detectCores() - 1
       doMC::registerDoMC(cores=ncores)
       is_parallel<-TRUE
       
      } else {
        is_parallel<-FALSE
      }
      
      vr_rules<-validateData(d,organisationUnit = input$ou,
                             datasets = ds,
                             parallel = is_parallel)
      
      #If there are any validation rule violations, put them in the output
      if  ( NROW(vr_rules) > 0 )  {
        messages<-append( paste("ERROR! :",  NROW(vr_rules)," validation rule violations found!"),messages)
       
        validation$validation_rules <- vr_rules[,c("name","ou_name","period","mech_code","formula")]
        has_error<-TRUE
      } else {
        messages<-append( "No validation rule violations found", messages)
        }
    })
    if (has_error) {
      shinyjs::show("downloadData")
    }
    
    list(data=d,messages=messages,validation=validation,has_error=has_error)
  }
  
  validation_results <- reactive({ validate() })
  
  output$downloadData <- downloadHandler(
    filename = "validation_results.xlsx",
    content = function(file) {
      
      vr_results <- validation_results() %>% purrr::pluck(.,"validation")
      openxlsx::write.xlsx(vr_results, file = file)
    }
  )
  
  output$contents <- renderDataTable({ 
    
    results<-validation_results() %>% 
      purrr::pluck(., "validation") %>% 
      purrr::pluck(., "validation_rules") 


    if ( inherits(results, "data.frame") ) { 
      results }
    else { NULL }
  })
  
  output$messages <- renderUI({
    
    vr<-validation_results() 
    
    messages<-NULL
    
    if ( is.null(vr)) {
      return(NULL)
    }
    
    if ( inherits(vr,"error") ) {
      return( paste0("ERROR! ",vr$message) )
      
    } else {
      
      messages<-vr %>%   
        purrr::pluck(., "messages")
      
      if (!is.null(messages))  {
        lapply(messages, function(x)
          tags$li(x))
      } else
      {
        tags$li("No issues found! Congratulations!")
      }
    }
  })
})
