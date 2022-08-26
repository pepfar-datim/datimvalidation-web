library(shiny)
library(shinyjs)
library(openxlsx)
library(magrittr)
require(futile.logger)
require(shinyWidgets)


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

  observeEvent(input$reset_input, {
    enableUI()
    ready$ok<-FALSE
    shinyjs::hide("downloadData")
  })

  observeEvent(input$logout,{
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated<-FALSE
    user_input$d2_session<-NULL
    session$reload()
    gc()

  } )

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
            selectInput("type", "Type:",
                        c(
                          "CSV" = "csv",
                          "JSON" = "json",
                          "XML" = "xml"
                        )),
            selectInput("ou", "Operating Unit", user_input$user_ous),
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
              c("Results" = "RESULTS", "Targets" = "TARGETS", "Narratives" = "NARRATIVES_RESULTS")
            ),
            checkboxInput("header", "CSV Header", TRUE),
            tags$hr(),
            actionButton("validate","Validate"),
            downloadButton("downloadData", "Download report"),
            tags$hr(),
            div(style = "display: inline-block; vertical-align:top; width: 80 px;", actionButton("reset_input", "Reset inputs")),
            div(style = "display: inline-block; vertical-align:top; width: 80 px;", actionButton("logout", "Logout"))
          ),
          mainPanel(tabsetPanel(
            type = "tabs",
            tabPanel("Messages",   tags$ul(uiOutput('messages'))),
            tabPanel("Validation rules", DT::DTOutput("contents"))
          ))
        ))
  }
})

  user_input <- reactiveValues(authenticated = FALSE,
                               status = "",
                               d2_session = NULL,
                               user_ous = NA)

  observeEvent(input$login_button,
               {

                 tryCatch(  {  datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
                                                        username = input$user_name,
                                                        password = input$password) },
                            #This function throws an error if the login is not successful
                            error=function(e) {
                              sendSweetAlert(
                                session,
                                title = "Login failed",
                                text = "Please check your username/password!",
                                type = "error")
                              flog.info(paste0("User ", input$user_name, " login failed."), name = "datapack")
                            } )

                 if ( exists("d2_default_session"))  {

                   user_input$authenticated<-TRUE
                   user_input$d2_session<-d2_default_session$clone()
                   flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged in."), name = "datapack")
                   foo<-getUserOperatingUnits(user_input$d2_session$user_orgunit, d2session = user_input$d2_session )
                   user_input$user_ous<-setNames(foo$id,foo$name)
                 }

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
    step_size <- 1/14
    withProgress(message = 'Validating file', value = 0,{

    incProgress(step_size, detail = ("Loading metadata"))
    ds<-getCurrentMERDataSets(type = input$ds_type, d2session = user_input$d2_session)
    incProgress(step_size, detail = ("Parsing data"))
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
        csv_header = input$header,
        d2session = user_input$d2_session
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

      #Common checks for all data
      #Period check
      incProgress(step_size, detail = ("Checking periods."))

        messages <-  append(
          paste(
            paste( "Periods found: ", paste(unique(d$period),sep="",collapse=","))
          ),messages )

        #Duplicates check
        incProgress(step_size, detail = ("Checking for duplicate records."))

        dup_check <- getExactDuplicates(d)

        if (inherits(dup_check, "data.frame") & NROW(dup_check) > 0) {
          messages <-  append(
            paste("ERROR! ",
                  paste( NROW(dup_check)," duplicate values found.")
            ),messages )

          validation$duplicates_check<-dup_check

          has_error<-TRUE
        } else {
          messages<-append("No duplicate records detected.",messages)
        }

        #Check that orgunits are in the provided user hierarchy
        incProgress(step_size, detail = ("Checking sites are within the operating unit"))
        ou_hierarchy_check <-  checkOrgunitsInHierarchy(d = d,
                                                        userOrgUnit = input$ou,
                                                        d2session = user_input$d2_session)

        if (inherits(ou_hierarchy_check, "list") && length(ou_hierarchy_check > 0L)) {
          messages<-append(paste("ERROR! ",
                                 length(ou_hierarchy_check),
                                 "organisation units found which were not in the provided operating unit!"
          ), messages)

          validation$ou_hierarchy_check<-data.frame(invalid_orgunits = ou_hierarchy_check)

          has_error<-TRUE
        } else {
          messages<-append("All organisation units were within the provided operating unit.", messages)
        }


        incProgress(step_size, detail = ("Checking data element/mechanism associations"))
        de_acoc_check <-
          checkDataElementMechValidity(
            d = d,
            datasets = ds,
            return_violations = TRUE,
            d2session = user_input$d2_session
          )

        #Check data elements and mechanism associations
        if (inherits(de_acoc_check, "data.frame") && NROW(de_acoc_check > 0L)) {
          messages<-append(paste("ERROR! ",
                                 NROW(de_acoc_check),
                                 "invalid data element/mechanism associations found!"
          ), messages)

          validation$dataelement_acoc_check<-de_acoc_check

          has_error<-TRUE
        } else {
          messages<-append("Data element/mechanism associations are valid.", messages)
        }

        #Check data element and organisation unit associations
        incProgress(step_size, detail = ("Checking data element/orgunit associations"))
        de_ou_check <-
          checkDataElementOrgunitValidity(
            d = d,
            datasets = ds,
            return_violations = TRUE,
            d2session = user_input$d2_session
          )

        if (inherits(de_ou_check, "data.frame") && NROW(de_ou_check > 0L)) {
          messages<-append(paste("ERROR! ",
                                 NROW(de_ou_check),
                                 "invalid data element/orgunit associations found!"
          ), messages)

          validation$dataelement_ou_check<-de_ou_check

          has_error<-TRUE
        } else {
          messages<-append("Data element/orgunit associations are valid.", messages)
        }

        #Data element cadence check
        incProgress(step_size, detail = ("Checking data elemement cadence"))
        de_cadence_check <- checkDataElementCadence(d, d2session = user_input$d2_session)

        if (inherits(de_cadence_check, "data.frame")) {

          messages <- append(paste("ERROR! ",
                                   NROW(de_cadence_check),
                                   "data elements were submitted for the incorrect period!"
          ),
          messages)

          validation$de_cadence_check<-de_cadence_check
          has_error<-TRUE
        } else {
          messages<-append("Data elements were submittted for the correct period.",messages)
        }

        #Check data element / category option combinations
        incProgress(step_size, detail = ("Checking data element/disagg associations"))

        ds_disagg_check <-
          checkDataElementDisaggValidity(d, datasets = ds,
                                         return_violations = TRUE,
                                         d2session = user_input$d2_session)

        if (inherits(ds_disagg_check, "data.frame")) {

          messages <- append(paste("ERROR! ",
                                   NROW(ds_disagg_check),
                                   "invalid data element/disagg associations found!"
          ),
          messages)

          validation$datasets_disagg_check<-ds_disagg_check
          has_error<-TRUE
        } else {
          messages<-append("Data element/disagg associations are valid.",messages)
        }

        #Mechanism check
        incProgress(step_size, detail = ("Checking mechanisms."))

        mech_check <-
          checkMechanismValidity(
            data = d,
            organisationUnit = input$ou,
            return_violations = TRUE,
            d2session = user_input$d2_session
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

        #Only for narratives

        if (input$ds_type %in% c("NARRATIVES_RESULTS")) {
          shinyjs::hide("contents")
          bad_narratives <- checkNarrativeLength(d)

          if (inherits(bad_narratives, "data.frame") &
              NROW(bad_narratives) > 0) {
            messages <-
              append(
                paste(
                  "ERROR! :",
                  NROW(bad_narratives),
                  " narratives with more than 50000 characters found."
                ),
                messages
              )
            validation$bad_narratives <- bad_narratives
            has_error <- TRUE
          } else {
            messages <- append("Narratvies are of an acceptable length.", messages)
          }
        }

        #Only for MER Results and Targets
        if (input$ds_type %in% c("RESULTS","TARGETS")) {
          #Zeros
          incProgress(step_size, detail = ("Checking zeros: "))

          zero_check<-sprintf("%1.2f%%",  ( sum(as.character(d$value) == "0") / NROW(d) )  * 100 )

          messages <-  append(
            paste(
              paste( "Records found: ", NROW(d), " records found of which ", zero_check, " were zeros.")
            ),messages )


        #Value type compliance check
        incProgress(step_size, detail = ("Checking value type compliance."))

        vt_check <- checkValueTypeCompliance(d, d2session = user_input$d2_session)

        if (inherits(vt_check, "data.frame") & NROW(vt_check) > 0) {
          messages <-  append( paste( "ERROR! :", NROW(vt_check)," invalid values found."), messages)
          validation$value_type_compliance<-vt_check
          has_error<-TRUE
        } else {
          messages<-append("Value types are valid.",messages)
        }

        #Negative value check
        incProgress(step_size, detail = ("Checking negative numbers."))

        neg_check <- checkNegativeValues(d, d2session = user_input$d2_session)

        if (inherits(neg_check, "data.frame")) {
          messages <- append(paste("ERROR! :", NROW(neg_check), " negatve values found."), messages)
          validation$negative_values <- neg_check
          has_error<-TRUE
        } else {
          messages<-append("No negative values found.",messages)
        }

        incProgress(step_size, detail = ("Checking validation rules..."))

        if ( Sys.info()['sysname'] == "Linux") {

          ncores <- parallel::detectCores() - 1
          doMC::registerDoMC(cores=ncores)
          is_parallel<-TRUE

        } else {
          is_parallel<-FALSE
        }

        vr_rules<-validateData(d,organisationUnit = input$ou,
                               parallel = is_parallel,
                               d2session = user_input$d2_session)

        #If there are any validation rule violations, put them in the output
        if  ( NROW(vr_rules) > 0 )  {
          messages<-append( paste("ERROR! :",  NROW(vr_rules)," validation rule violations found!"),messages)

          validation$validation_rules <- vr_rules[,c("name","ou_name","period","mech_code","formula")]
          has_error<-TRUE
        } else {
          messages<-append( "No validation rule violations found", messages)
        }
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

  output$contents <- DT::renderDT({

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
